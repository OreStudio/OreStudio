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
#include "ores.qt/ReturnDistributionChart.hpp"
#include "ores.qt/SamplePricePathsChart.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QAbstractItemView>
#include <QButtonGroup>
#include <QComboBox>
#include <QCompleter>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QPalette>
#include <QHeaderView>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QToolButton>
#include <QSignalBlocker>
#include <QSizePolicy>
#include <QSlider>
#include <QStackedWidget>
#include <QTableWidget>
#include <QTextBrowser>
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

// Base volatility (fraction of log-price) used by the volatility profiles/sliders.
constexpr double v0 = 0.0005;

// Simple-mode slider ranges (slider int 0..100 maps to these per-update spans,
// expressed in raw increment units).
constexpr double kDriftMin = -0.005; // -0.5% per update
constexpr double kDriftMax = 0.005;  // +0.5% per update
constexpr double kVolMin = 0.0;      // 0% per update
constexpr double kVolMax = 0.02;     // 2% per update
constexpr double kJumpMin = 0.0;     // weight of the jump component
constexpr double kJumpMax = 0.5;     // up to 50% mixture share

// Advanced table columns (no per-component Type — engine is config-level now).
enum Col { ColName = 0, ColProfile = 1, ColMean = 2, ColStdev = 3, ColWeight = 4, ColActions = 5 };

double sliderToValue(int slider, double lo, double hi) {
    return lo + (hi - lo) * (slider / 100.0);
}

int valueToSlider(double value, double lo, double hi) {
    if (hi <= lo)
        return 0;
    const double t = (value - lo) / (hi - lo);
    return std::clamp(static_cast<int>(std::lround(t * 100.0)), 0, 100);
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
    , isNew_(true)
    , userEditedSource_(false) // new config: auto-derive the source name from the pair
{

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

    // Seed a sensible default single GBM process (Normal volatility).
    components_.push_back(ModelComponent{{}, "Primary process", 0.0, v0 * 2, 1.0});

    buildUi();

    syncAdvancedFromModel();
    syncSimpleFromModel();
    refreshCharts();

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

    // Build the model (source of truth) from the loaded components.
    auto sorted = components;
    std::sort(sorted.begin(), sorted.end(), [](const auto& a, const auto& b) {
        return a.component_index < b.component_index;
    });
    for (const auto& c : sorted) {
        originalComponentIds_.push_back(boost::uuids::to_string(c.id));
        components_.push_back(ModelComponent{boost::uuids::to_string(c.id), c.description,
                                             c.mean, c.stdev, c.weight});
    }

    buildUi();

    syncAdvancedFromModel();
    syncSimpleFromModel();
    refreshCharts();

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

void FxSpotRateEditor::buildBehaviourTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(8);

    // ===== 1. Header (full width) =====
    auto* intro = new QLabel(
        tr("How the price moves on each update. The increment distribution is shaped by "
           "one or more components; volatility scales roughly with √time. Use Simple for "
           "a quick feel, or Advanced for full component control."),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray;");
    layout->addWidget(intro);

    // Engine combo (left) + update-frequency control + segmented toggle (right).
    auto* headerRow = new QHBoxLayout();
    headerRow->addWidget(new QLabel(tr("Engine:"), tab));
    engineCombo_ = new QComboBox(tab);
    engineCombo_->addItem(tr("Geometric Brownian Motion"), QStringLiteral("geometric"));
    engineCombo_->addItem(tr("Arithmetic Brownian Motion"), QStringLiteral("arithmetic"));
    engineCombo_->setToolTip(
        tr("The price-process engine. Geometric uses log-returns (stays positive); "
           "arithmetic uses absolute price changes (symmetric, may go negative)."));
    {
        const int idx = engineCombo_->findData(QString::fromStdString(fx_.process_type));
        engineCombo_->setCurrentIndex(idx >= 0 ? idx : 0);
    }
    headerRow->addWidget(engineCombo_);

    // Update-frequency control, folded in from the former "Update frequency" tab.
    headerRow->addSpacing(16);
    headerRow->addWidget(new QLabel(tr("New price every"), tab));
    secondsSpin_ = new QSpinBox(tab);
    secondsSpin_->setRange(1, 3600);
    secondsSpin_->setSuffix(tr(" s"));
    secondsSpin_->setToolTip(
        tr("How often a new price is generated — the simulation's clock (tick clock)."));
    {
        const int seconds = fx_.ticks_per_hour > 0
            ? std::max(1, static_cast<int>(std::lround(3600.0 / fx_.ticks_per_hour)))
            : 1;
        secondsSpin_->setValue(seconds);
    }
    headerRow->addWidget(secondsSpin_);
    frequencyEchoLabel_ = new QLabel(tab);
    frequencyEchoLabel_->setStyleSheet("color: gray;");
    headerRow->addWidget(frequencyEchoLabel_);
    connect(secondsSpin_, &QSpinBox::valueChanged, this,
            [this](int) { recomputeFrequencyEcho(); });

    headerRow->addStretch(1);

    // Prominent segmented Simple/Advanced toggle (right).
    auto* simpleBtn = new QPushButton(tr("Simple"), tab);
    auto* advancedBtn = new QPushButton(tr("Advanced"), tab);
    const QColor accent = palette().color(QPalette::Highlight);
    const QColor accentText = palette().color(QPalette::HighlightedText);
    const QString segStyle =
        QStringLiteral(
            "QPushButton { min-height: 30px; min-width: 110px; font-weight: bold; "
            "padding: 4px 16px; border: 1px solid %1; }"
            "QPushButton:checked { background: %1; color: %2; }")
            .arg(accent.name(), accentText.name());
    simpleBtn->setStyleSheet(
        segStyle + "QPushButton { border-top-right-radius: 0; border-bottom-right-radius: 0; }");
    advancedBtn->setStyleSheet(
        segStyle + "QPushButton { border-top-left-radius: 0; border-bottom-left-radius: 0; "
                   "border-left: none; }");
    for (auto* b : {simpleBtn, advancedBtn}) {
        b->setCheckable(true);
        b->setAutoExclusive(true);
        b->setCursor(Qt::PointingHandCursor);
    }
    simpleBtn->setChecked(true);
    modeGroup_ = new QButtonGroup(this);
    modeGroup_->setExclusive(true);
    modeGroup_->addButton(simpleBtn, 0);
    modeGroup_->addButton(advancedBtn, 1);

    auto* segRow = new QHBoxLayout();
    segRow->setSpacing(0); // connected segments
    segRow->addWidget(simpleBtn);
    segRow->addWidget(advancedBtn);
    headerRow->addLayout(segRow);
    layout->addLayout(headerRow);

    // Inline, non-blocking warning shown only for the arithmetic engine (full width).
    engineWarningLabel_ = new QLabel(
        tr("⚠ Arithmetic engine: μ and σ are absolute price increments (not "
           "%/log-returns), and the price can go negative or zero — unrealistic for an "
           "FX rate. Intended for testing the process abstraction."),
        tab);
    engineWarningLabel_->setWordWrap(true);
    engineWarningLabel_->setStyleSheet("color:#d08020;");
    engineWarningLabel_->setVisible(currentEngine() == "arithmetic");
    layout->addWidget(engineWarningLabel_);

    connect(engineCombo_, &QComboBox::currentIndexChanged, this,
            [this](int) { onEngineChanged(); });

    // ===== Single shared charts =====
    distChart_ = new ReturnDistributionChart(tab);
    pathsChart_ = new SamplePricePathsChart(clientManager_, tab);

    // ===== 2. Middle row — HORIZONTAL: controls (left) | PDF chart (right) =====
    auto* middleRow = new QHBoxLayout();
    middleRow->setSpacing(12);

    // LEFT: mode stack (controls only). Dominant horizontal stretch.
    modeStack_ = new QStackedWidget(tab);
    modeStack_->addWidget(buildSimpleControls());   // index 0
    modeStack_->addWidget(buildAdvancedControls()); // index 1
    middleRow->addWidget(modeStack_, 1);

    // RIGHT: compact PDF chart group, top-aligned.
    auto* distBox = new QGroupBox(tr("Return distribution"), tab);
    distBox->setMinimumWidth(300);
    distBox->setMaximumWidth(380);
    auto* distLayout = new QVBoxLayout(distBox);
    distLayout->setContentsMargins(12, 12, 12, 12);
    distChart_->setMinimumWidth(300);
    distChart_->setMaximumWidth(380);
    distChart_->setMinimumHeight(240);
    distLayout->addWidget(distChart_);
    middleRow->addWidget(distBox, 0, Qt::AlignTop);

    layout->addLayout(middleRow);

    // ===== 3. Bottom (full width) — prominent Sample paths chart =====
    auto* pathsBox = new QGroupBox(tr("Sample paths"), tab);
    auto* pathsLayout = new QVBoxLayout(pathsBox);
    pathsLayout->setContentsMargins(12, 12, 12, 12);
    pathsChart_->setMinimumHeight(340);
    pathsChart_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    pathsLayout->addWidget(pathsChart_);
    layout->addWidget(pathsBox, 1);

    connect(modeGroup_, &QButtonGroup::idClicked, this, [this](int) { onModeChanged(); });

    tabWidget_->addTab(tab, tr("Price behaviour"));
}

QWidget* FxSpotRateEditor::buildSimpleControls() {
    auto* paramsBox = new QGroupBox(tr("Parameters"), this);
    paramsBox->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* paramsLayout = new QVBoxLayout(paramsBox);
    paramsLayout->setContentsMargins(12, 12, 12, 12);
    paramsLayout->setSpacing(8);

    const auto makeSlider = [&](const QString& title, const QString& tip, QSlider*& slider,
                                QLabel*& valueLabel) {
        auto* header = new QHBoxLayout();
        auto* titleLabel = new QLabel(title, paramsBox);
        auto* info = new QToolButton(paramsBox);
        info->setText(QStringLiteral("ⓘ"));
        info->setAutoRaise(true);
        info->setCursor(Qt::PointingHandCursor);
        info->setToolTip(tip);
        info->setStyleSheet(
            "QToolButton { border: none; padding: 0 4px; color: #3a8ee6; font-weight: bold; }"
            "QToolButton:hover { color: #66b0ff; }");
        connect(info, &QToolButton::clicked, this, [this, title, tip]() {
            QMessageBox::information(this, title, tip);
        });
        header->addWidget(titleLabel);
        header->addWidget(info);
        header->addStretch(1);
        valueLabel = new QLabel(paramsBox);
        valueLabel->setStyleSheet("color: gray;");
        header->addWidget(valueLabel);
        paramsLayout->addLayout(header);

        slider = new QSlider(Qt::Horizontal, paramsBox);
        slider->setRange(0, 100);
        paramsLayout->addWidget(slider);
    };

    makeSlider(tr("Global Trend Drift"),
               tr("Average direction per update (log-return drift)."), driftSlider_,
               driftValueLabel_);
    makeSlider(tr("Global Volatility"),
               tr("Typical size of each move (log-return volatility)."), volSlider_,
               volValueLabel_);
    makeSlider(tr("Jump Event Frequency"),
               tr("Approximate frequency of large jumps (modelled as a wide mixture "
                  "component — a GMM approximation, not a Poisson jump process)."),
               jumpSlider_, jumpValueLabel_);

    auto* note = new QLabel(
        tr("Switch to Advanced mode for direct GMM component control (μ, σ, w)."),
        paramsBox);
    note->setWordWrap(true);
    note->setStyleSheet("color: gray;");
    paramsLayout->addWidget(note);

    auto* resetRow = new QHBoxLayout();
    auto* resetBtn = new QPushButton(tr("Reset"), paramsBox);
    resetBtn->setToolTip(tr("Restore default parameters (no trend, Normal volatility, "
                            "no jumps)."));
    connect(resetBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onResetSimple);
    resetRow->addWidget(resetBtn);
    resetRow->addStretch(1);
    paramsLayout->addLayout(resetRow);
    paramsLayout->addStretch(1);

    for (auto* s : {driftSlider_, volSlider_, jumpSlider_})
        connect(s, &QSlider::valueChanged, this, [this](int) {
            if (syncing_)
                return;
            rebuildModelFromSimple();
            refreshCharts(); // recompute PDF + debounced sample-paths refresh
        });

    return paramsBox;
}

QWidget* FxSpotRateEditor::buildAdvancedControls() {
    auto* compBox = new QGroupBox(tr("Gaussian Mixture Model Components"), this);
    compBox->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* compLayout = new QVBoxLayout(compBox);
    compLayout->setContentsMargins(12, 12, 12, 12);
    compLayout->setSpacing(8);

    componentTable_ = new QTableWidget(0, 6, compBox);
    componentTable_->setHorizontalHeaderLabels(
        {tr("Component Name"), tr("Profile"), tr("Drift (μ %)"), tr("Volatility (σ %)"),
         tr("Weight"), tr("Actions")});
    // (A "Jump (planned)" column is intentionally omitted — not backed.)
    componentTable_->setMinimumWidth(560);
    componentTable_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    componentTable_->setShowGrid(false); // avoid cell-border + inner-widget "box in box"
    componentTable_->verticalHeader()->setVisible(false);
    componentTable_->verticalHeader()->setDefaultSectionSize(38); // roomier rows
    {
        // Name column stretches to fill; the rest size to their contents.
        auto* hdr = componentTable_->horizontalHeader();
        hdr->setStretchLastSection(false);
        hdr->setSectionResizeMode(ColName, QHeaderView::Stretch);
        for (int col : {ColProfile, ColMean, ColStdev, ColWeight, ColActions})
            hdr->setSectionResizeMode(col, QHeaderView::ResizeToContents);
    }
    compLayout->addWidget(componentTable_);

    auto* compButtons = new QHBoxLayout();
    auto* addBtn = new QPushButton(tr("Add process"), compBox);
    addBtn->setToolTip(tr("Add another process — two or more processes form a regime mix."));
    connect(addBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onAddComponentRow);
    compButtons->addWidget(addBtn);
    auto* resetAdvBtn = new QPushButton(tr("Reset"), compBox);
    resetAdvBtn->setToolTip(tr("Restore a single Normal-profile component."));
    connect(resetAdvBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onResetAdvanced);
    compButtons->addWidget(resetAdvBtn);
    compButtons->addStretch(1);
    weightSumLabel_ = new QLabel(compBox);
    weightSumLabel_->setStyleSheet("color: gray;");
    compButtons->addWidget(weightSumLabel_);
    compLayout->addLayout(compButtons);

    // Future tools tucked behind a disclosure (collapsed by default).
    auto* futureBox = new QGroupBox(tr("Component Tools"), compBox);
    futureBox->setCheckable(true);
    futureBox->setChecked(false);
    auto* futureLayout = new QVBoxLayout(futureBox);
    auto* futureText = new QTextBrowser(futureBox);
    futureText->setOpenExternalLinks(false);
    futureText->setText(
        tr("Planned — not yet available:\n"
           "• Jump / Poisson processes\n"
           "• Custom jump distributions\n"
           "• Cross-rate correlation matrices\n"
           "These are placeholders for future capability and are not wired to anything."));
    futureText->setMaximumHeight(120);
    futureLayout->addWidget(futureText);
    // Hide the body until the user expands the box.
    futureText->setVisible(false);
    connect(futureBox, &QGroupBox::toggled, futureText, &QWidget::setVisible);
    compLayout->addWidget(futureBox);

    return compBox;
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
                // Derive the default source name once currencies are known
                // (no-op if the user has already edited it, or in edit mode).
                self->recomputeDefaultSourceName();
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

namespace {

// Volatility profile combo entries.
const QStringList kProfiles = {QStringLiteral("Custom"), QStringLiteral("Flat"),
                               QStringLiteral("Calm"), QStringLiteral("Normal"),
                               QStringLiteral("Volatile")};

// Map a raw stdev to the profile that would have produced it, else Custom.
QString profileForStdev(double stdev) {
    constexpr double eps = 1e-12;
    if (std::abs(stdev - 0.0) < eps)
        return QStringLiteral("Flat");
    if (std::abs(stdev - v0 * 0.5) < eps)
        return QStringLiteral("Calm");
    if (std::abs(stdev - v0 * 2) < eps)
        return QStringLiteral("Normal");
    if (std::abs(stdev - v0 * 8) < eps)
        return QStringLiteral("Volatile");
    return QStringLiteral("Custom");
}

double stdevForProfile(const QString& profile, double current) {
    if (profile == "Flat")
        return 0.0;
    if (profile == "Calm")
        return v0 * 0.5;
    if (profile == "Normal")
        return v0 * 2;
    if (profile == "Volatile")
        return v0 * 8;
    return current; // Custom
}

}

void FxSpotRateEditor::addTableRow(const ModelComponent& c) {
    const int row = componentTable_->rowCount();
    componentTable_->insertRow(row);

    // A flat, frameless look so the inner widgets don't draw a heavy border
    // inside the cell grid (avoids the "box in box" effect).
    const QString flatEdit = QStringLiteral("border: none; background: transparent;");

    // Description; the component id is stashed as a property on the name widget.
    auto* nameEdit = new QLineEdit(QString::fromStdString(c.description), componentTable_);
    nameEdit->setPlaceholderText(tr("Description"));
    nameEdit->setProperty("componentId", QString::fromStdString(c.id));
    nameEdit->setMinimumWidth(150);
    nameEdit->setFrame(false);
    nameEdit->setStyleSheet(flatEdit);
    nameEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);

    auto* profileCombo = new QComboBox(componentTable_);
    profileCombo->addItems(kProfiles);
    profileCombo->setCurrentText(profileForStdev(c.stdev));
    // Size the cell and the drop-down popup to the longest entry ("Volatile")
    // so labels are never truncated to "Vol...ile".
    profileCombo->setSizeAdjustPolicy(QComboBox::AdjustToContents);
    profileCombo->setMinimumContentsLength(8);
    profileCombo->view()->setMinimumWidth(
        profileCombo->view()->sizeHintForColumn(0) + 16);
    profileCombo->setStyleSheet(QStringLiteral("QComboBox { border: none; background: "
                                               "transparent; }"));

    auto* meanSpin = new QDoubleSpinBox(componentTable_);
    meanSpin->setRange(-100.0, 100.0);
    meanSpin->setDecimals(3);
    meanSpin->setSuffix(tr(" %"));
    meanSpin->setValue(c.mean * 100.0);
    meanSpin->setFrame(false);
    meanSpin->setStyleSheet(flatEdit);
    meanSpin->setToolTip(tr("Average %1 per update (%); 0 = no drift.").arg(incrementNoun()));

    auto* stdevSpin = new QDoubleSpinBox(componentTable_);
    stdevSpin->setRange(0.0, 100.0);
    stdevSpin->setDecimals(3);
    stdevSpin->setSuffix(tr(" %"));
    stdevSpin->setValue(c.stdev * 100.0);
    stdevSpin->setFrame(false);
    stdevSpin->setStyleSheet(flatEdit);
    stdevSpin->setToolTip(tr("Volatility of the %1 per update (%); 0 = constant.")
                              .arg(incrementNoun()));

    auto* weightSpin = new QDoubleSpinBox(componentTable_);
    weightSpin->setRange(0.0, 1e6);
    weightSpin->setDecimals(3);
    weightSpin->setValue(c.weight);
    weightSpin->setFrame(false);
    weightSpin->setStyleSheet(flatEdit);
    weightSpin->setToolTip(tr("Relative share when blending processes (normalised on save)."));

    // Icon-only trash button, centred in the Actions cell via a small container.
    auto* removeBtn = new QPushButton(componentTable_);
    removeBtn->setIcon(IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    removeBtn->setToolTip(tr("Remove"));
    removeBtn->setFixedSize(26, 26);
    removeBtn->setFlat(true);
    auto* actionsCell = new QWidget(componentTable_);
    auto* actionsLayout = new QHBoxLayout(actionsCell);
    actionsLayout->setContentsMargins(4, 2, 4, 2);
    actionsLayout->setSpacing(0);
    actionsLayout->addWidget(removeBtn, 0, Qt::AlignCenter);

    componentTable_->setCellWidget(row, ColName, nameEdit);
    componentTable_->setCellWidget(row, ColProfile, profileCombo);
    componentTable_->setCellWidget(row, ColMean, meanSpin);
    componentTable_->setCellWidget(row, ColStdev, stdevSpin);
    componentTable_->setCellWidget(row, ColWeight, weightSpin);
    componentTable_->setCellWidget(row, ColActions, actionsCell);

    // Profile: fill σ (σ's signal is blocked inside applyProfileToRow).
    connect(profileCombo, &QComboBox::currentTextChanged, this,
            [this, profileCombo](const QString& p) {
                for (int r = 0; r < componentTable_->rowCount(); ++r) {
                    if (componentTable_->cellWidget(r, ColProfile) == profileCombo) {
                        applyProfileToRow(r, p);
                        break;
                    }
                }
                if (syncing_)
                    return;
                rebuildModelFromAdvanced();
                refreshCharts();
            });
    // Manual σ edit flips profile to Custom.
    connect(stdevSpin, &QDoubleSpinBox::valueChanged, this, [this, profileCombo](double) {
        {
            const QSignalBlocker blocker(profileCombo);
            profileCombo->setCurrentText(QStringLiteral("Custom"));
        }
        if (syncing_)
            return;
        rebuildModelFromAdvanced();
        refreshCharts();
    });
    connect(meanSpin, &QDoubleSpinBox::valueChanged, this, [this](double) {
        if (syncing_)
            return;
        rebuildModelFromAdvanced();
        refreshCharts();
    });
    connect(weightSpin, &QDoubleSpinBox::valueChanged, this, [this](double) {
        if (syncing_)
            return;
        rebuildModelFromAdvanced();
        refreshCharts();
    });
    connect(nameEdit, &QLineEdit::textEdited, this, [this](const QString&) {
        if (syncing_)
            return;
        rebuildModelFromAdvanced();
    });
    connect(removeBtn, &QPushButton::clicked, this, [this, nameEdit]() {
        if (componentTable_->rowCount() <= 1)
            return; // keep at least one component
        for (int r = 0; r < componentTable_->rowCount(); ++r) {
            if (componentTable_->cellWidget(r, ColName) == nameEdit) {
                componentTable_->removeRow(r);
                break;
            }
        }
        rebuildModelFromAdvanced();
        updateRemoveButtonsEnabled();
        refreshCharts();
    });

    updateRemoveButtonsEnabled();
}

void FxSpotRateEditor::applyProfileToRow(int row, const QString& profile) {
    if (profile == "Custom")
        return;
    auto* stdevSpin = qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(row, ColStdev));
    if (!stdevSpin)
        return;
    const double sigma = stdevForProfile(profile, stdevSpin->value() / 100.0);
    const QSignalBlocker blocker(stdevSpin); // don't bounce profile back to Custom
    stdevSpin->setValue(sigma * 100.0);
}

std::string FxSpotRateEditor::currentEngine() const {
    return engineCombo_ ? engineCombo_->currentData().toString().toStdString() : "geometric";
}

QString FxSpotRateEditor::incrementNoun() const {
    return currentEngine() == "arithmetic" ? tr("price change") : tr("log-return");
}

void FxSpotRateEditor::onEngineChanged() {
    if (engineCombo_)
        fx_.process_type = currentEngine();
    if (engineWarningLabel_)
        engineWarningLabel_->setVisible(currentEngine() == "arithmetic");
    // Engine only affects the path simulation, not the increment PDF.
    refreshCharts();
}

void FxSpotRateEditor::onAddComponentRow() {
    addTableRow(ModelComponent{{}, "Process", 0.0, v0 * 2, 1.0});
    rebuildModelFromAdvanced();
    updateRemoveButtonsEnabled();
    refreshCharts();
}

void FxSpotRateEditor::updateRemoveButtonsEnabled() {
    const bool canRemove = componentTable_->rowCount() > 1;
    for (int r = 0; r < componentTable_->rowCount(); ++r) {
        // The Actions cell holds a container; find the icon button within it.
        if (auto* cell = componentTable_->cellWidget(r, ColActions)) {
            if (auto* btn = cell->findChild<QPushButton*>())
                btn->setEnabled(canRemove);
        }
    }
}

void FxSpotRateEditor::onResetSimple() {
    syncing_ = true;
    driftSlider_->setValue(valueToSlider(0.0, kDriftMin, kDriftMax));   // no trend
    volSlider_->setValue(valueToSlider(v0 * 2, kVolMin, kVolMax));      // Normal volatility
    jumpSlider_->setValue(valueToSlider(0.0, kJumpMin, kJumpMax));      // no jumps
    syncing_ = false;
    rebuildModelFromSimple();
    refreshCharts();
}

void FxSpotRateEditor::onResetAdvanced() {
    components_ = {ModelComponent{{}, "Primary process", 0.0, v0 * 2, 1.0}};
    syncing_ = true;
    syncAdvancedFromModel();
    syncing_ = false;
    updateRemoveButtonsEnabled();
    refreshCharts();
}

std::vector<FxSpotRateEditor::ModelComponent> FxSpotRateEditor::currentComponents() const {
    return components_;
}

void FxSpotRateEditor::syncAdvancedFromModel() {
    const QSignalBlocker blocker(componentTable_);
    componentTable_->setRowCount(0);
    for (const auto& c : components_)
        addTableRow(c);
    updateRemoveButtonsEnabled();
    // Refresh weight-sum label.
    double sum = 0.0;
    for (const auto& c : components_)
        sum += c.weight;
    weightSumLabel_->setText(tr("Weight Sum = %1 (normalised on save)").arg(sum, 0, 'f', 3));
}

void FxSpotRateEditor::rebuildModelFromAdvanced() {
    std::vector<ModelComponent> next;
    for (int r = 0; r < componentTable_->rowCount(); ++r) {
        auto* nameEdit = qobject_cast<QLineEdit*>(componentTable_->cellWidget(r, ColName));
        auto* meanSpin = qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColMean));
        auto* stdevSpin =
            qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColStdev));
        auto* weightSpin =
            qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColWeight));
        if (!nameEdit || !meanSpin || !stdevSpin || !weightSpin)
            continue;
        ModelComponent c;
        c.id = nameEdit->property("componentId").toString().toStdString();
        c.description = nameEdit->text().toStdString();
        c.mean = meanSpin->value() / 100.0;
        c.stdev = stdevSpin->value() / 100.0;
        c.weight = weightSpin->value();
        next.push_back(c);
    }
    components_ = std::move(next);

    double sum = 0.0;
    for (const auto& c : components_)
        sum += c.weight;
    weightSumLabel_->setText(tr("Weight Sum = %1 (normalised on save)").arg(sum, 0, 'f', 3));
}

void FxSpotRateEditor::syncSimpleFromModel() {
    if (components_.empty())
        return;

    // Primary = the highest-weight component; jump = sum of weights of components
    // whose σ ≥ 4× the primary σ.
    std::size_t primary = 0;
    for (std::size_t i = 1; i < components_.size(); ++i)
        if (components_[i].weight > components_[primary].weight)
            primary = i;

    const double drift = components_[primary].mean;
    const double vol = components_[primary].stdev;
    double jumpWeight = 0.0;
    double totalWeight = 0.0;
    for (const auto& c : components_)
        totalWeight += c.weight;
    for (std::size_t i = 0; i < components_.size(); ++i) {
        if (i == primary)
            continue;
        if (components_[i].stdev >= 4.0 * vol)
            jumpWeight += components_[i].weight;
    }
    const double jumpFraction = totalWeight > 0.0 ? jumpWeight / totalWeight : 0.0;

    syncing_ = true;
    driftSlider_->setValue(valueToSlider(drift, kDriftMin, kDriftMax));
    volSlider_->setValue(valueToSlider(vol, kVolMin, kVolMax));
    jumpSlider_->setValue(valueToSlider(jumpFraction, kJumpMin, kJumpMax));
    syncing_ = false;

    driftValueLabel_->setText(tr("%1 % / update").arg(drift * 100.0, 0, 'g', 3));
    volValueLabel_->setText(tr("%1 % / update").arg(vol * 100.0, 0, 'g', 3));
    jumpValueLabel_->setText(tr("%1 %").arg(jumpFraction * 100.0, 0, 'g', 3));
}

void FxSpotRateEditor::rebuildModelFromSimple() {
    const double drift = sliderToValue(driftSlider_->value(), kDriftMin, kDriftMax);
    const double vol = sliderToValue(volSlider_->value(), kVolMin, kVolMax);
    const double jumpFraction = sliderToValue(jumpSlider_->value(), kJumpMin, kJumpMax);

    driftValueLabel_->setText(tr("%1 % / update").arg(drift * 100.0, 0, 'g', 3));
    volValueLabel_->setText(tr("%1 % / update").arg(vol * 100.0, 0, 'g', 3));
    jumpValueLabel_->setText(tr("%1 %").arg(jumpFraction * 100.0, 0, 'g', 3));

    // Preserve the existing primary component's id (so edits update in place).
    std::string primaryId;
    std::string jumpId;
    if (!components_.empty()) {
        std::size_t primary = 0;
        for (std::size_t i = 1; i < components_.size(); ++i)
            if (components_[i].weight > components_[primary].weight)
                primary = i;
        primaryId = components_[primary].id;
        for (std::size_t i = 0; i < components_.size(); ++i)
            if (i != primary && components_[i].stdev >= 4.0 * components_[primary].stdev)
                jumpId = components_[i].id; // reuse one jump id if present
    }

    std::vector<ModelComponent> next;
    next.push_back(ModelComponent{primaryId, "Primary process", drift, vol,
                                  1.0 - jumpFraction});
    // Jump regime: a wider component (σ ≈ 6× the base volatility). It scales with
    // vol with no baseline floor, so at vol≈0 the jump collapses to ~0 width and
    // cannot dominate the distribution.
    if (jumpFraction > 0.0) {
        next.push_back(ModelComponent{jumpId, "Jump regime (approx.)", 0.0, vol * 6.0,
                                      jumpFraction});
    }
    components_ = std::move(next);

    // Reflect into the advanced table so the two surfaces stay consistent.
    syncing_ = true;
    syncAdvancedFromModel();
    syncing_ = false;
}

void FxSpotRateEditor::refreshCharts() {
    std::vector<ReturnDistributionChart::Component> distComps;
    std::vector<SamplePricePathsChart::Component> pathComps;
    double sum = 0.0;
    for (const auto& c : components_)
        sum += c.weight;
    for (const auto& c : components_) {
        const double w = sum > 0.0 ? c.weight / sum : c.weight;
        distComps.push_back({c.mean, c.stdev, w});
        pathComps.push_back({c.mean, c.stdev, w});
    }

    const double price = priceSpin_ ? priceSpin_->value() : fx_.gmm_initial_price;
    const std::string engine = currentEngine();

    if (distChart_)
        distChart_->setComponents(distComps); // PDF is engine-independent (increment dist)
    if (pathsChart_) {
        pathsChart_->setComponents(pathComps);
        pathsChart_->setInitialPrice(price);
        pathsChart_->setProcessType(engine);
        pathsChart_->scheduleRefresh();
    }
}

void FxSpotRateEditor::onModeChanged() {
    const int id = modeGroup_ ? modeGroup_->checkedId() : 0;
    // Reflect the latest values from the page we're leaving into the model, then
    // populate the page we're switching to.
    if (id == 0) {
        syncSimpleFromModel();
    } else {
        syncing_ = true;
        syncAdvancedFromModel();
        syncing_ = false;
    }
    modeStack_->setCurrentIndex(id);
    refreshCharts();
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

    // Make sure the model reflects whichever surface is currently active.
    if (modeGroup_ && modeGroup_->checkedId() == 1)
        rebuildModelFromAdvanced();
    else
        rebuildModelFromSimple();

    // Reject an all-zero weight set: the server builds a std::discrete_distribution
    // from the weights, which is undefined behaviour when they sum to zero.
    {
        double weightSum = 0.0;
        for (const auto& mc : components_)
            weightSum += mc.weight;
        if (weightSum <= 0.0) {
            QMessageBox::warning(
                this, tr("Invalid weights"),
                tr("All component weights are zero. At least one component must "
                   "have a positive weight."));
            return;
        }
    }

    // Build the fx_spot config.
    auto fx = fx_;
    fx.base_currency_code = base;
    fx.quote_currency_code = quote;
    fx.ore_key = "FX/RATE/" + base + "/" + quote;
    fx.source_name = sourceNameEdit_->text().trimmed().toStdString();
    if (fx.source_name.empty())
        fx.source_name = defaultSourceName().toStdString();
    fx.process_type = currentEngine();
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
    for (const auto& mc : components_)
        total += mc.weight;

    std::vector<synthetic::domain::gmm_component> comps;
    std::vector<std::string> keptIds;
    int index = 0;
    for (const auto& mc : components_) {
        synthetic::domain::gmm_component c;
        const bool rowIsNew = mc.id.empty();
        c.id = boost::uuids::random_generator()();
        if (!rowIsNew) {
            try {
                c.id = boost::lexical_cast<boost::uuids::uuid>(mc.id);
            } catch (...) {
                c.id = boost::uuids::random_generator()();
            }
        }
        c.fx_spot_config_id = fx.id;
        c.party_id = clientManager_->currentPartyId();
        c.component_index = index++;
        c.description = mc.description;
        c.mean = mc.mean;
        c.stdev = mc.stdev;
        c.weight = total > 0.0 ? mc.weight / total : mc.weight;
        c.modified_by = username_.toStdString();
        c.change_reason_code = rowIsNew ? "system.new_record" : "common.non_material_update";
        c.change_commentary = "Authored via Market Simulator";
        c.version = 0;
        comps.push_back(c);
        if (!mc.id.empty())
            keptIds.push_back(mc.id);
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
