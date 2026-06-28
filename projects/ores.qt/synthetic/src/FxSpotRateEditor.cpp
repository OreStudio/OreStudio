/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/FxSpotRateEditor.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QCompleter>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QScrollArea>
#include <QSignalBlocker>
#include <QtConcurrent>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <utility>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {

// Base volatility (fraction of log-price) used to scale the presets.
constexpr double v0 = 0.0005;

std::string to_lower(const std::string& s) {
    std::string r = s;
    std::transform(r.begin(), r.end(), r.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return r;
}

// Lowercase the feed name and replace runs of non-alphanumerics with '-'.
std::string slug(const QString& name) {
    std::string out;
    bool prevDash = false;
    for (const QChar qc : name) {
        const char c = static_cast<char>(qc.toLatin1());
        if (std::isalnum(static_cast<unsigned char>(c))) {
            out += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
            prevDash = false;
        } else if (!prevDash) {
            out += '-';
            prevDash = true;
        }
    }
    // Trim leading/trailing dashes.
    while (!out.empty() && out.front() == '-')
        out.erase(out.begin());
    while (!out.empty() && out.back() == '-')
        out.pop_back();
    return out;
}

}

FxSpotRateEditor::FxSpotRateEditor(ClientManager* cm,
                                   ImageCache* imageCache,
                                   ChangeReasonCache* crCache,
                                   const QString& username,
                                   const boost::uuids::uuid& parentFeedId,
                                   const QString& feedName,
                                   QWidget* parent)
    : DetailDialogBase(parent)
    , clientManager_(cm)
    , imageCache_(imageCache)
    , username_(username)
    , feedName_(feedName)
    , isNew_(true) {

    setChangeReasonCache(crCache);

    fx_.id = boost::uuids::random_generator()();
    fx_.config_id = parentFeedId;
    fx_.party_id = cm->currentPartyId();
    fx_.gmm_initial_price = 1.0;
    fx_.ticks_per_hour = 3600; // default: a new price every second
    fx_.enabled = true;

    BOOST_LOG_SEV(lg(), info) << "Opening FX spot rate editor for a new rate "
                              << boost::uuids::to_string(fx_.id) << " under feed "
                              << boost::uuids::to_string(parentFeedId) << ".";

    buildUi();
    setProvenanceEnabled(false);
}

FxSpotRateEditor::FxSpotRateEditor(
    ClientManager* cm,
    ImageCache* imageCache,
    ChangeReasonCache* crCache,
    const QString& username,
    const synthetic::domain::fx_spot_generation_config& existing,
    const QString& feedName,
    const std::vector<synthetic::domain::gmm_component>& components,
    QWidget* parent)
    : DetailDialogBase(parent)
    , clientManager_(cm)
    , imageCache_(imageCache)
    , username_(username)
    , feedName_(feedName)
    , isNew_(false)
    , userEditedSource_(true) // existing source name is authoritative; don't overwrite
    , fx_(existing) {

    setChangeReasonCache(crCache);

    BOOST_LOG_SEV(lg(), info) << "Opening FX spot rate editor editing rate "
                              << boost::uuids::to_string(fx_.id) << " with " << components.size()
                              << " components.";

    buildUi();

    // Load the component stack.
    auto sorted = components;
    std::sort(sorted.begin(), sorted.end(), [](const auto& a, const auto& b) {
        return a.component_index < b.component_index;
    });
    for (const auto& c : sorted) {
        originalComponentIds_.push_back(boost::uuids::to_string(c.id));
        addComponentRow(QString::fromStdString(c.description), c.mean, c.stdev, c.weight,
                        boost::uuids::to_string(c.id));
    }
    recomputeWeightSum();

    populateProvenance(fx_.version, fx_.modified_by, fx_.performed_by, fx_.recorded_at,
                       fx_.change_reason_code, fx_.change_commentary);
    setProvenanceEnabled(true);
}

void FxSpotRateEditor::buildUi() {
    setWindowTitle(isNew_ ? tr("New FX Rate") : tr("Edit FX Rate"));

    auto* layout = new QVBoxLayout(this);
    tabWidget_ = new QTabWidget(this);
    layout->addWidget(tabWidget_, 1);

    buildInstrumentTab();
    buildFrequencyTab();
    buildBehaviourTab();

    // Provenance tab (DetailDialogBase contract).
    provenanceTab_ = new QWidget(this);
    auto* provLayout = new QVBoxLayout(provenanceTab_);
    provenanceWidget_ = new ProvenanceWidget(provenanceTab_);
    provLayout->addWidget(provenanceWidget_);
    provLayout->addStretch(1);
    tabWidget_->addTab(provenanceTab_, tr("Provenance"));

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Save | QDialogButtonBox::Close, this);
    layout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &FxSpotRateEditor::onSaveClicked);
    connect(buttons, &QDialogButtonBox::rejected, this, [this]() { onCloseClicked(); });

    populateCurrencyCombo(baseCombo_);
    populateCurrencyCombo(quoteCombo_);
    recomputeOreKey();
    recomputeFrequencyEcho();
}

void FxSpotRateEditor::buildInstrumentTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);

    auto* intro = new QLabel(
        tr("Pick the currency pair to simulate. The ORE key and a default source name "
           "are derived; the source name is editable."),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray; font-style: italic;");
    layout->addWidget(intro);

    auto* form = new QFormLayout();

    baseCombo_ = new QComboBox(tab);
    quoteCombo_ = new QComboBox(tab);
    for (auto* combo : {baseCombo_, quoteCombo_}) {
        combo->setEditable(true);
        combo->setInsertPolicy(QComboBox::NoInsert);
        combo->completer()->setCompletionMode(QCompleter::PopupCompletion);
        combo->completer()->setFilterMode(Qt::MatchContains);
        combo->completer()->setCaseSensitivity(Qt::CaseInsensitive);
    }

    oreKeyLabel_ = new QLabel(tab);
    oreKeyLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);

    sourceNameEdit_ = new QLineEdit(tab);
    sourceNameEdit_->setText(QString::fromStdString(fx_.source_name));

    priceSpin_ = new QDoubleSpinBox(tab);
    priceSpin_->setRange(0.0001, 1e9);
    priceSpin_->setDecimals(4);
    priceSpin_->setValue(fx_.gmm_initial_price > 0 ? fx_.gmm_initial_price : 1.0);
    priceSpin_->setToolTip(tr("The spot rate the simulation starts from."));

    enabledCheck_ = new QCheckBox(tr("Enabled"), tab);
    enabledCheck_->setChecked(fx_.enabled);

    form->addRow(tr("Base currency"), baseCombo_);
    form->addRow(tr("Quote currency"), quoteCombo_);
    form->addRow(tr("ORE key"), oreKeyLabel_);
    form->addRow(tr("Source name"), sourceNameEdit_);
    form->addRow(tr("Initial price"), priceSpin_);
    form->addRow(QString(), enabledCheck_);
    layout->addLayout(form);
    layout->addStretch(1);

    tabWidget_->addTab(tab, tr("Instrument"));

    connect(baseCombo_, &QComboBox::currentTextChanged, this,
            &FxSpotRateEditor::onCurrencyChanged);
    connect(quoteCombo_, &QComboBox::currentTextChanged, this,
            &FxSpotRateEditor::onCurrencyChanged);
    connect(baseCombo_, &QComboBox::currentIndexChanged, this,
            &FxSpotRateEditor::onCurrencyChanged);
    connect(quoteCombo_, &QComboBox::currentIndexChanged, this,
            &FxSpotRateEditor::onCurrencyChanged);
    connect(sourceNameEdit_, &QLineEdit::textEdited, this,
            [this](const QString&) { userEditedSource_ = true; });
}

void FxSpotRateEditor::buildFrequencyTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);

    auto* intro = new QLabel(
        tr("How often a new price is generated — the simulation's clock. "
           "(technical: tick clock)"),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray; font-style: italic;");
    layout->addWidget(intro);

    auto* form = new QFormLayout();
    secondsSpin_ = new QSpinBox(tab);
    secondsSpin_->setRange(1, 3600);
    secondsSpin_->setSuffix(tr(" seconds"));
    const int seconds = fx_.ticks_per_hour > 0
        ? std::max(1, static_cast<int>(std::lround(3600.0 / fx_.ticks_per_hour)))
        : 1;
    secondsSpin_->setValue(seconds);
    form->addRow(tr("New price every"), secondsSpin_);
    layout->addLayout(form);

    frequencyEchoLabel_ = new QLabel(tab);
    frequencyEchoLabel_->setStyleSheet("color: gray;");
    layout->addWidget(frequencyEchoLabel_);
    layout->addStretch(1);

    tabWidget_->addTab(tab, tr("Update frequency"));

    connect(secondsSpin_, &QSpinBox::valueChanged, this,
            [this](int) { recomputeFrequencyEcho(); });
}

void FxSpotRateEditor::buildBehaviourTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);

    auto* intro = new QLabel(
        tr("How the price moves on each update — a blend of bell curves (a Gaussian "
           "Mixture Model). Each component is one bell curve; they stack to shape the "
           "behaviour. Pick a preset or edit the stack."),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray; font-style: italic;");
    layout->addWidget(intro);

    // Preset buttons.
    auto* presetRow = new QHBoxLayout();
    for (const auto* name : {"Flat", "Brownian (Wiener)", "GBM drift", "Calm", "Normal",
                             "Volatile", "Regime mix"}) {
        auto* btn = new QPushButton(tr(name), tab);
        const QString preset = QString::fromUtf8(name);
        connect(btn, &QPushButton::clicked, this, [this, preset]() { applyPreset(preset); });
        presetRow->addWidget(btn);
    }
    presetRow->addStretch(1);
    layout->addLayout(presetRow);

    // Scrollable stack of component rows.
    auto* scroll = new QScrollArea(tab);
    scroll->setWidgetResizable(true);
    auto* stackHost = new QWidget(scroll);
    stackLayout_ = new QVBoxLayout(stackHost);
    stackLayout_->addStretch(1);
    scroll->setWidget(stackHost);
    layout->addWidget(scroll, 1);

    auto* bottomRow = new QHBoxLayout();
    auto* addBtn = new QPushButton(tr("Add component"), tab);
    connect(addBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onAddComponent);
    bottomRow->addWidget(addBtn);
    bottomRow->addStretch(1);
    weightSumLabel_ = new QLabel(tab);
    weightSumLabel_->setStyleSheet("color: gray;");
    bottomRow->addWidget(weightSumLabel_);
    layout->addLayout(bottomRow);

    tabWidget_->addTab(tab, tr("Price behaviour"));

    recomputeWeightSum();
}

void FxSpotRateEditor::populateCurrencyCombo(QComboBox* combo) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<FxSpotRateEditor> self = this;
    QPointer<QComboBox> target = combo;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        return fetch_currency_codes(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self,
            [self, target, watcher]() {
                auto codes = watcher->result();
                watcher->deleteLater();
                if (!self || !target)
                    return;

                std::sort(codes.begin(), codes.end());
                self->knownCodes_ = codes;

                const QString preselect = (target == self->baseCombo_)
                    ? QString::fromStdString(self->fx_.base_currency_code)
                    : QString::fromStdString(self->fx_.quote_currency_code);

                const QSignalBlocker blocker(target);
                target->clear();
                target->addItem(QString()); // "(select)" sentinel
                for (const auto& code : codes)
                    target->addItem(QString::fromStdString(code));

                setup_flag_combo(self, target, self->imageCache_, FlagSource::Currency);

                if (!preselect.isEmpty())
                    target->setCurrentText(preselect);
                else
                    target->setCurrentIndex(0);

                self->recomputeOreKey();
            });

    watcher->setFuture(QtConcurrent::run(task));
}

QString FxSpotRateEditor::defaultSourceName() const {
    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();
    if (base.empty() || quote.empty())
        return {};
    return QString::fromStdString("ores.synthetic." + slug(feedName_) + "." + to_lower(base) +
                                  "." + to_lower(quote));
}

void FxSpotRateEditor::recomputeOreKey() {
    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();
    if (base.empty() || quote.empty())
        oreKeyLabel_->setText({});
    else
        oreKeyLabel_->setText(QString::fromStdString("FX/RATE/" + base + "/" + quote));
}

void FxSpotRateEditor::recomputeDefaultSourceName() {
    if (userEditedSource_)
        return;
    const auto def = defaultSourceName();
    if (!def.isEmpty()) {
        const QSignalBlocker blocker(sourceNameEdit_);
        sourceNameEdit_->setText(def);
    }
}

void FxSpotRateEditor::onCurrencyChanged() {
    recomputeOreKey();
    recomputeDefaultSourceName();
}

void FxSpotRateEditor::recomputeFrequencyEcho() {
    const int seconds = std::max(1, secondsSpin_->value());
    const int ticks = std::max(1, static_cast<int>(std::lround(3600.0 / seconds)));
    frequencyEchoLabel_->setText(tr("= %1 ticks/hour").arg(ticks));
}

void FxSpotRateEditor::recomputeWeightSum() {
    double sum = 0.0;
    for (const auto& r : rows_)
        sum += r.weightSpin->value();
    weightSumLabel_->setText(tr("weights sum = %1 (normalised on save)").arg(sum, 0, 'f', 4));
}

void FxSpotRateEditor::clearComponentRows() {
    for (auto& r : rows_) {
        stackLayout_->removeWidget(r.container);
        r.container->deleteLater();
    }
    rows_.clear();
}

void FxSpotRateEditor::addComponentRow(const QString& desc, double mean, double stdev,
                                       double weight, const std::string& id) {
    auto* container = new QWidget();
    auto* row = new QHBoxLayout(container);
    row->setContentsMargins(0, 0, 0, 0);

    auto* descEdit = new QLineEdit(desc, container);
    descEdit->setPlaceholderText(tr("Description"));

    auto* meanSpin = new QDoubleSpinBox(container);
    meanSpin->setRange(-1e9, 1e9);
    meanSpin->setDecimals(6);
    meanSpin->setValue(mean);
    meanSpin->setToolTip(tr("Average per-tick drift; usually 0."));

    auto* stdevSpin = new QDoubleSpinBox(container);
    stdevSpin->setRange(0.0000001, 1e9);
    stdevSpin->setDecimals(7);
    stdevSpin->setValue(stdev > 0 ? stdev : v0);
    stdevSpin->setToolTip(tr("Volatility per tick, e.g. 0.001; larger = choppier."));

    auto* weightSpin = new QDoubleSpinBox(container);
    weightSpin->setRange(0.0, 1e9);
    weightSpin->setDecimals(4);
    weightSpin->setValue(weight);
    weightSpin->setToolTip(
        tr("Relative proportion of this component; weights are normalised across all "
           "components."));

    auto* removeBtn = new QPushButton(tr("Remove"), container);

    row->addWidget(descEdit, 2);
    row->addWidget(new QLabel(tr("μ"), container));
    row->addWidget(meanSpin, 1);
    row->addWidget(new QLabel(tr("σ"), container));
    row->addWidget(stdevSpin, 1);
    row->addWidget(new QLabel(tr("w"), container));
    row->addWidget(weightSpin, 1);
    row->addWidget(removeBtn);

    // Insert before the trailing stretch.
    stackLayout_->insertWidget(stackLayout_->count() - 1, container);

    ComponentRow cr{container, descEdit, meanSpin, stdevSpin, weightSpin, id};
    rows_.push_back(cr);

    connect(weightSpin, &QDoubleSpinBox::valueChanged, this,
            [this](double) { recomputeWeightSum(); });
    connect(removeBtn, &QPushButton::clicked, this, [this, container]() {
        auto it = std::find_if(rows_.begin(), rows_.end(),
                               [container](const ComponentRow& r) {
                                   return r.container == container;
                               });
        if (it != rows_.end()) {
            stackLayout_->removeWidget(it->container);
            it->container->deleteLater();
            rows_.erase(it);
        }
        recomputeWeightSum();
    });

    recomputeWeightSum();
}

void FxSpotRateEditor::onAddComponent() {
    addComponentRow(tr("Component"), 0.0, v0, 1.0);
}

void FxSpotRateEditor::applyPreset(const QString& preset) {
    struct Comp {
        const char* desc;
        double mean;
        double stdev;
        double weight;
    };
    std::vector<Comp> comps;

    if (preset == "Flat")
        comps = {{"Constant", 0.0, 1e-7, 1.0}};
    else if (preset == "Brownian (Wiener)")
        comps = {{"Driftless GBM", 0.0, v0, 1.0}};
    else if (preset == "GBM drift")
        comps = {{"GBM with drift", 0.00001, v0, 1.0}};
    else if (preset == "Calm")
        comps = {{"Calm", 0.0, v0 * 0.5, 1.0}};
    else if (preset == "Normal")
        comps = {{"Normal", 0.0, v0 * 2, 1.0}};
    else if (preset == "Volatile")
        comps = {{"Volatile", 0.0, v0 * 8, 1.0}};
    else if (preset == "Regime mix")
        comps = {{"Quiet regime", 0.0, v0, 0.8}, {"Stress regime", 0.0, v0 * 6, 0.2}};

    BOOST_LOG_SEV(lg(), info) << "Applying price-behaviour preset '" << preset.toStdString()
                              << "' with " << comps.size() << " components.";

    clearComponentRows();
    for (const auto& c : comps)
        addComponentRow(tr(c.desc), c.mean, c.stdev, c.weight);
    recomputeWeightSum();
}

void FxSpotRateEditor::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        QMessageBox::warning(this, tr("Disconnected"),
                             tr("Cannot save while disconnected from the server."));
        return;
    }

    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();

    if (base.empty() || quote.empty()) {
        QMessageBox::warning(this, tr("Incomplete"),
                             tr("Both base and quote currencies must be set."));
        return;
    }
    if (base == quote) {
        QMessageBox::warning(this, tr("Invalid pair"),
                             tr("Base and quote currencies must differ."));
        return;
    }
    const auto isKnown = [this](const std::string& c) {
        return std::find(knownCodes_.begin(), knownCodes_.end(), c) != knownCodes_.end();
    };
    if (!isKnown(base) || !isKnown(quote)) {
        QMessageBox::warning(this, tr("Unknown currency"),
                             tr("Both base and quote must be valid currency codes."));
        return;
    }

    const auto crOpType = isNew_ ? ChangeReasonDialog::OperationType::Create
                                 : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, true, isNew_ ? "system" : "common");
    if (!crSel)
        return;

    // Build the fx_spot config.
    auto fx = fx_;
    fx.base_currency_code = base;
    fx.quote_currency_code = quote;
    fx.ore_key = "FX/RATE/" + base + "/" + quote;
    fx.source_name = sourceNameEdit_->text().trimmed().toStdString();
    if (fx.source_name.empty())
        fx.source_name = defaultSourceName().toStdString();
    fx.gmm_initial_price = priceSpin_->value();
    fx.ticks_per_hour =
        std::max(1, static_cast<int>(std::lround(3600.0 / std::max(1, secondsSpin_->value()))));
    fx.enabled = enabledCheck_->isChecked();
    fx.party_id = clientManager_->currentPartyId();
    fx.modified_by = username_.toStdString();
    fx.change_reason_code = crSel->reason_code;
    fx.change_commentary =
        crSel->commentary.empty() ? "Authored via Market Simulator" : crSel->commentary;
    fx.version = 0;

    // Build the component stack, normalising weights to sum 1.
    double total = 0.0;
    for (const auto& r : rows_)
        total += r.weightSpin->value();

    std::vector<synthetic::domain::gmm_component> comps;
    std::vector<std::string> keptIds;
    int index = 0;
    for (const auto& r : rows_) {
        synthetic::domain::gmm_component c;
        const bool rowIsNew = r.id.empty();
        c.id = rowIsNew ? boost::uuids::random_generator()()
                        : boost::uuids::random_generator()(); // placeholder; replaced below
        if (!rowIsNew) {
            try {
                c.id = boost::lexical_cast<boost::uuids::uuid>(r.id);
            } catch (...) {
                c.id = boost::uuids::random_generator()();
            }
        }
        c.fx_spot_config_id = fx.id;
        c.party_id = clientManager_->currentPartyId();
        c.component_index = index++;
        c.description = r.descEdit->text().toStdString();
        c.mean = r.meanSpin->value();
        c.stdev = r.stdevSpin->value();
        c.weight = total > 0.0 ? r.weightSpin->value() / total : r.weightSpin->value();
        c.modified_by = username_.toStdString();
        c.change_reason_code = rowIsNew ? "system.new_record" : "common.non_material_update";
        c.change_commentary = "Authored via Market Simulator";
        c.version = 0;
        comps.push_back(c);
        if (!r.id.empty())
            keptIds.push_back(r.id);
    }

    // Components present originally but no longer in the UI must be deleted.
    std::vector<std::string> toDelete;
    for (const auto& origId : originalComponentIds_) {
        if (std::find(keptIds.begin(), keptIds.end(), origId) == keptIds.end())
            toDelete.push_back(origId);
    }

    const std::string fxId = boost::uuids::to_string(fx.id);
    BOOST_LOG_SEV(lg(), info) << "Saving FX spot rate " << fxId << " (" << base << "/" << quote
                              << ", new=" << isNew_ << ") with " << comps.size()
                              << " components, deleting " << toDelete.size() << ".";

    QPointer<FxSpotRateEditor> self = this;
    auto* cm = clientManager_;

    struct SaveResult {
        bool success;
        QString message;
    };

    auto task = [cm, fx, comps, toDelete]() -> SaveResult {
        namespace m = synthetic::messaging;

        auto fxResp =
            cm->process_authenticated_request(m::save_fx_spot_generation_config_request::from(fx));
        if (!fxResp)
            return {false, QString::fromStdString(fxResp.error())};
        if (!fxResp->success)
            return {false, QString::fromStdString(fxResp->message)};

        for (const auto& c : comps) {
            auto cResp =
                cm->process_authenticated_request(m::save_gmm_component_request::from(c));
            if (!cResp)
                return {false, QString::fromStdString(cResp.error())};
            if (!cResp->success)
                return {false, QString::fromStdString(cResp->message)};
        }

        if (!toDelete.empty()) {
            auto dResp = cm->process_authenticated_request(
                m::delete_gmm_component_request{.ids = toDelete});
            if (!dResp)
                return {false, QString::fromStdString(dResp.error())};
            if (!dResp->success)
                return {false, QString::fromStdString(dResp->message)};
        }

        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher, fxId]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Save failed for FX spot rate " << fxId << ": " << result.message.toStdString();
            emit self->errorOccurred(result.message);
            QMessageBox::critical(self, self->tr("Save failed"), result.message);
            return;
        }
        BOOST_LOG_SEV(lg(), info) << "Saved FX spot rate " << fxId << ".";
        emit self->savedOk();
        emit self->statusChanged(self->tr("FX rate saved."));
        self->notifySaveSuccess(self->tr("FX rate saved."));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}
