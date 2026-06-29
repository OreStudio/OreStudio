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
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QCompleter>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QGroupBox>
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

// Base volatility (fraction of log-price) used by the volatility profiles.
constexpr double v0 = 0.0005;

// Volatility profile names (combo entries).
const char* const kProfileCustom = "Custom";
const char* const kProfileFlat = "Flat";
const char* const kProfileCalm = "Calm";
const char* const kProfileNormal = "Normal";
const char* const kProfileVolatile = "Volatile";

// Type combo entries.
const char* const kTypeDriftless = "Geometric Brownian Motion";
const char* const kTypeWithDrift = "Geometric Brownian Motion with drift";

// Map a stdev value to the profile that would have produced it (else Custom).
QString profileForStdev(double stdev) {
    constexpr double eps = 1e-12;
    if (std::abs(stdev - 0.0) < eps)
        return kProfileFlat;
    if (std::abs(stdev - v0 * 0.5) < eps)
        return kProfileCalm;
    if (std::abs(stdev - v0 * 2) < eps)
        return kProfileNormal;
    if (std::abs(stdev - v0 * 8) < eps)
        return kProfileVolatile;
    return kProfileCustom;
}

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
        const ProcessType type =
            (c.mean != 0.0) ? ProcessType::GbmWithDrift : ProcessType::DriftlessGbm;
        const QString profile = profileForStdev(c.stdev);
        addProcessCard(type, profile, c.mean, c.stdev, c.weight,
                       QString::fromStdString(c.description), boost::uuids::to_string(c.id));
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
    if (auto* saveBtn = buttons->button(QDialogButtonBox::Save)) {
        saveBtn->setIcon(IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
        saveBtn->setText(tr("Save"));
    }
    if (auto* closeBtn = buttons->button(QDialogButtonBox::Close)) {
        closeBtn->setIcon(
            IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
        closeBtn->setText(tr("Close"));
    }
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
        tr("The price follows one or more stochastic processes, blended together. One "
           "process is a simple model; add more to stack different regimes (a regime "
           "mix). For each process pick its type and a volatility profile."),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray;");
    layout->addWidget(intro);

    // Scrollable stack of process cards.
    auto* scroll = new QScrollArea(tab);
    scroll->setWidgetResizable(true);
    auto* stackHost = new QWidget(scroll);
    stackLayout_ = new QVBoxLayout(stackHost);
    stackLayout_->addStretch(1);
    scroll->setWidget(stackHost);
    layout->addWidget(scroll, 1);

    auto* bottomRow = new QHBoxLayout();
    auto* addBtn = new QPushButton(tr("Add process"), tab);
    connect(addBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onAddProcess);
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
    for (const auto& c : cards_)
        sum += c.weightSpin->value();
    weightSumLabel_->setText(tr("weights sum = %1 (normalised on save)").arg(sum, 0, 'f', 4));
}

void FxSpotRateEditor::clearProcessCards() {
    for (auto& c : cards_) {
        stackLayout_->removeWidget(c.container);
        c.container->deleteLater();
    }
    cards_.clear();
}

void FxSpotRateEditor::renumberCards() {
    for (std::size_t i = 0; i < cards_.size(); ++i) {
        if (auto* gb = qobject_cast<QGroupBox*>(cards_[i].container))
            gb->setTitle(tr("Process %1").arg(i + 1));
    }
}

void FxSpotRateEditor::applyProfileToCard(ProcessCard& card, const QString& profile) {
    if (profile == kProfileCustom)
        return;

    double sigma = card.stdevSpin->value();
    if (profile == kProfileFlat)
        sigma = 0.0;
    else if (profile == kProfileCalm)
        sigma = v0 * 0.5;
    else if (profile == kProfileNormal)
        sigma = v0 * 2;
    else if (profile == kProfileVolatile)
        sigma = v0 * 8;

    // Block σ's signal so setting it does not flip the profile back to Custom.
    const QSignalBlocker blocker(card.stdevSpin);
    card.stdevSpin->setValue(sigma);
}

void FxSpotRateEditor::applyTypeToCard(ProcessCard& card) {
    const bool withDrift = card.typeCombo->currentIndex() ==
        static_cast<int>(ProcessType::GbmWithDrift);
    card.driftRow->setVisible(withDrift);
    if (!withDrift) {
        const QSignalBlocker blocker(card.meanSpin);
        card.meanSpin->setValue(0.0);
    }
}

void FxSpotRateEditor::addProcessCard(ProcessType type, const QString& profile, double mean,
                                      double stdev, double weight, const QString& desc,
                                      const std::string& id) {
    auto* box = new QGroupBox();
    auto* boxLayout = new QVBoxLayout(box);

    // Header row: title placeholder + Remove (right-aligned).
    auto* headerRow = new QHBoxLayout();
    auto* removeBtn = new QPushButton(tr("Remove"), box);
    headerRow->addStretch(1);
    headerRow->addWidget(removeBtn);
    boxLayout->addLayout(headerRow);

    auto* form = new QFormLayout();

    auto* typeCombo = new QComboBox(box);
    typeCombo->addItem(tr(kTypeDriftless));
    typeCombo->addItem(tr(kTypeWithDrift));
    typeCombo->setCurrentIndex(static_cast<int>(type));

    auto* profileCombo = new QComboBox(box);
    for (const auto* p :
         {kProfileCustom, kProfileFlat, kProfileCalm, kProfileNormal, kProfileVolatile})
        profileCombo->addItem(tr(p));
    {
        const int idx = profileCombo->findText(profile);
        profileCombo->setCurrentIndex(idx >= 0 ? idx : 0);
    }

    auto* meanSpin = new QDoubleSpinBox(box);
    meanSpin->setRange(-1e9, 1e9);
    meanSpin->setDecimals(6);
    meanSpin->setValue(mean);
    meanSpin->setToolTip(tr("Average drift per tick"));

    auto* stdevSpin = new QDoubleSpinBox(box);
    stdevSpin->setRange(0.0, 1e9);
    stdevSpin->setDecimals(7);
    stdevSpin->setValue(stdev);
    stdevSpin->setToolTip(tr("Volatility per tick; 0 = constant"));

    auto* weightSpin = new QDoubleSpinBox(box);
    weightSpin->setRange(0.0, 1e9);
    weightSpin->setDecimals(4);
    weightSpin->setValue(weight);
    weightSpin->setToolTip(
        tr("Relative share when blending processes (normalised on save)"));

    auto* descEdit = new QLineEdit(desc, box);
    descEdit->setPlaceholderText(tr("Description"));

    // Drift row is shown/hidden as a unit.
    auto* driftRow = new QWidget(box);
    auto* driftRowLayout = new QHBoxLayout(driftRow);
    driftRowLayout->setContentsMargins(0, 0, 0, 0);
    driftRowLayout->addWidget(meanSpin);

    form->addRow(tr("Type"), typeCombo);
    form->addRow(tr("Volatility profile"), profileCombo);
    form->addRow(tr("Drift (μ)"), driftRow);
    form->addRow(tr("Volatility (σ)"), stdevSpin);
    form->addRow(tr("Weight (mix share)"), weightSpin);
    form->addRow(tr("Description"), descEdit);
    boxLayout->addLayout(form);

    // Insert before the trailing stretch.
    stackLayout_->insertWidget(stackLayout_->count() - 1, box);

    ProcessCard card{box, typeCombo, profileCombo, driftRow, meanSpin,
                     stdevSpin, weightSpin, descEdit, id};
    cards_.push_back(card);

    // Initial drift visibility based on type.
    applyTypeToCard(cards_.back());

    connect(typeCombo, &QComboBox::currentIndexChanged, this, [this, box](int) {
        auto it = std::find_if(cards_.begin(), cards_.end(),
                               [box](const ProcessCard& c) { return c.container == box; });
        if (it != cards_.end())
            applyTypeToCard(*it);
    });
    connect(profileCombo, &QComboBox::currentTextChanged, this,
            [this, box](const QString& text) {
                auto it =
                    std::find_if(cards_.begin(), cards_.end(),
                                 [box](const ProcessCard& c) { return c.container == box; });
                if (it != cards_.end())
                    applyProfileToCard(*it, text);
            });
    connect(stdevSpin, &QDoubleSpinBox::valueChanged, this, [this, box](double) {
        // Manual σ edit flips the profile to Custom.
        auto it = std::find_if(cards_.begin(), cards_.end(),
                               [box](const ProcessCard& c) { return c.container == box; });
        if (it != cards_.end()) {
            const QSignalBlocker blocker(it->profileCombo);
            it->profileCombo->setCurrentText(tr(kProfileCustom));
        }
    });
    connect(weightSpin, &QDoubleSpinBox::valueChanged, this,
            [this](double) { recomputeWeightSum(); });
    connect(removeBtn, &QPushButton::clicked, this, [this, box]() {
        auto it = std::find_if(cards_.begin(), cards_.end(),
                               [box](const ProcessCard& c) { return c.container == box; });
        if (it != cards_.end()) {
            stackLayout_->removeWidget(it->container);
            it->container->deleteLater();
            cards_.erase(it);
        }
        renumberCards();
        recomputeWeightSum();
    });

    renumberCards();
    recomputeWeightSum();
}

void FxSpotRateEditor::onAddProcess() {
    addProcessCard(ProcessType::DriftlessGbm, tr(kProfileNormal), 0.0, v0 * 2, 1.0,
                   tr("Process"));
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

    // Build the process stack, normalising weights to sum 1.
    double total = 0.0;
    for (const auto& card : cards_)
        total += card.weightSpin->value();

    std::vector<synthetic::domain::gmm_component> comps;
    std::vector<std::string> keptIds;
    int index = 0;
    for (const auto& card : cards_) {
        synthetic::domain::gmm_component c;
        const bool rowIsNew = card.id.empty();
        c.id = boost::uuids::random_generator()();
        if (!rowIsNew) {
            try {
                c.id = boost::lexical_cast<boost::uuids::uuid>(card.id);
            } catch (...) {
                c.id = boost::uuids::random_generator()();
            }
        }
        const bool withDrift = card.typeCombo->currentIndex() ==
            static_cast<int>(ProcessType::GbmWithDrift);
        c.fx_spot_config_id = fx.id;
        c.party_id = clientManager_->currentPartyId();
        c.component_index = index++;
        c.description = card.descEdit->text().toStdString();
        c.mean = withDrift ? card.meanSpin->value() : 0.0;
        c.stdev = card.stdevSpin->value();
        c.weight =
            total > 0.0 ? card.weightSpin->value() / total : card.weightSpin->value();
        c.modified_by = username_.toStdString();
        c.change_reason_code = rowIsNew ? "system.new_record" : "common.non_material_update";
        c.change_commentary = "Authored via Market Simulator";
        c.version = 0;
        comps.push_back(c);
        if (!card.id.empty())
            keptIds.push_back(card.id);
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
