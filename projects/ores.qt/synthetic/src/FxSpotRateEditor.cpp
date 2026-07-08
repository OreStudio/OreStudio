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
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/ReturnDistributionChart.hpp"
#include "ores.qt/SamplePricePathsChart.hpp"
#include "ores.synthetic.api/domain/process_parameter_validation.hpp"
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
#include <QHeaderView>
#include <QMessageBox>
#include <QPalette>
#include <QPointer>
#include <QPushButton>
#include <QRadioButton>
#include <QSignalBlocker>
#include <QSizePolicy>
#include <QSlider>
#include <QStackedWidget>
#include <QTableWidget>
#include <QTextBrowser>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <optional>
#include <utility>

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

/**
 * @brief Price-process engine metadata: the single source of truth for what
 * appears in the engine combo and how the editor's generic gating (Add
 * process / Simple mode / weight normalisation / the all-zero-weight guard)
 * behaves per engine — the UI asks "does this engine support mixing?"
 * rather than hardcoding "is this the ou engine?" throughout. More single-
 * regime engines (e.g. Vasicek/CIR/Hull-White for rates) are expected to
 * follow the same shape: add a row here, not new special-cases in the UI.
 */
struct EngineInfo {
    const char* code;  // matches process_type / process_factory's dispatch key
    const char* label; // combo box display text (wrapped in tr() at use site)
    // False for single-regime processes (e.g. mean-reverting): the Advanced
    // component table collapses to one row, "Add process" is disabled, the
    // all-zero-weight guard and weight-sum-to-1 normalisation are skipped
    // (that row's Weight field is repurposed as a scalar parameter, not a
    // mixture share). Per-engine field remapping/wording stays engine-
    // specific — a boolean can't express what a field means, only whether
    // there can be more than one of them.
    bool supportsMixing;
};
constexpr EngineInfo kEngines[] = {
    {"geometric", QT_TR_NOOP("Geometric Brownian Motion"), true},
    {"arithmetic", QT_TR_NOOP("Arithmetic Brownian Motion"), true},
    {"ou", QT_TR_NOOP("Ornstein-Uhlenbeck"), false},
};

const EngineInfo* find_engine(const std::string& code) {
    for (const auto& e : kEngines)
        if (code == e.code)
            return &e;
    return nullptr;
}

// Advanced table columns (no per-component Type — engine is config-level now).
enum Col {
    ColColor = 0,
    ColName = 1,
    ColProfile = 2,
    ColMean = 3,
    ColStdev = 4,
    ColWeight = 5,
    ColActions = 6
};

double sliderToValue(int slider, double lo, double hi) {
    return lo + (hi - lo) * (slider / 100.0);
}

int valueToSlider(double value, double lo, double hi) {
    if (hi <= lo)
        return 0;
    const double t = (value - lo) / (hi - lo);
    return std::clamp(static_cast<int>(std::lround(t * 100.0)), 0, 100);
}

// κ realistically spans several orders of magnitude (minutes- to months-long
// half-lives), so a linear slider is useless — most of its range would map to
// indistinguishably-fast reversion. Map it log-scale instead; 0 is a special
// case (no reversion) pinned to the low end rather than -infinity.
constexpr double kKappaMin = 1e-6;
constexpr double kKappaMax = 1.0;

double kappaSliderToValue(int slider) {
    const double t = slider / 100.0;
    return std::pow(10.0,
                    std::log10(kKappaMin) + t * (std::log10(kKappaMax) - std::log10(kKappaMin)));
}

int kappaValueToSlider(double kappa) {
    if (kappa <= kKappaMin)
        return 0;
    const double k = std::min(kappa, kKappaMax);
    const double t =
        (std::log10(k) - std::log10(kKappaMin)) / (std::log10(kKappaMax) - std::log10(kKappaMin));
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
    fx_.price_source = "fixed";
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

FxSpotRateEditor::FxSpotRateEditor(ClientManager* cm,
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
        components_.push_back(ModelComponent{
            boost::uuids::to_string(c.id), c.description, c.mean, c.stdev, c.weight});
    }

    buildUi();

    syncAdvancedFromModel();
    syncSimpleFromModel();
    refreshCharts();

    populateProvenance(fx_.version,
                       fx_.modified_by,
                       fx_.performed_by,
                       fx_.recorded_at,
                       fx_.change_reason_code,
                       fx_.change_commentary);
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

    auto* intro =
        new QLabel(tr("Pick the currency pair to simulate. The ORE key and a default source name "
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
    // "ou" echoes this as θ on its Simple page — keep that (and both charts,
    // which also read this as the OU level / GBM starting price) live.
    connect(priceSpin_, &QDoubleSpinBox::valueChanged, this, [this](double) {
        if (syncing_)
            return;
        if (currentEngine() == "ou" && ouThetaLabel_)
            ouThetaLabel_->setText(tr("%1").arg(priceSpin_->value(), 0, 'f', 5));
        refreshCharts();
    });

    vintageSourceEdit_ = new QLineEdit(tab);
    vintageSourceEdit_->setText(QString::fromStdString(fx_.vintage_source));
    vintageSourceEdit_->setPlaceholderText(tr("e.g. ore.reference"));
    vintageDateEdit_ = new QLineEdit(tab);
    vintageDateEdit_->setText(QString::fromStdString(fx_.vintage_date));
    vintageDateEdit_->setPlaceholderText(tr("YYYY-MM-DD"));

    auto* fixedRadio = new QRadioButton(tr("Fixed"), tab);
    auto* vintageRadio = new QRadioButton(tr("From vintage data"), tab);
    priceSourceGroup_ = new QButtonGroup(this);
    priceSourceGroup_->setExclusive(true);
    priceSourceGroup_->addButton(fixedRadio, 0);
    priceSourceGroup_->addButton(vintageRadio, 1);
    (fx_.price_source == "vintage" ? vintageRadio : fixedRadio)->setChecked(true);
    auto* priceSourceRow = new QHBoxLayout();
    priceSourceRow->addWidget(fixedRadio);
    priceSourceRow->addWidget(vintageRadio);
    priceSourceRow->addStretch(1);

    const auto updatePriceSourceEnablement = [this]() {
        const bool vintage = priceSourceGroup_->checkedId() == 1;
        priceSpin_->setEnabled(!vintage);
        vintageSourceEdit_->setEnabled(vintage);
        vintageDateEdit_->setEnabled(vintage);
    };
    updatePriceSourceEnablement();
    connect(priceSourceGroup_,
           &QButtonGroup::idClicked,
           this,
           [updatePriceSourceEnablement](int) { updatePriceSourceEnablement(); });

    enabledCheck_ = new QCheckBox(tr("Enabled"), tab);
    enabledCheck_->setChecked(fx_.enabled);

    form->addRow(tr("Base currency"), baseCombo_);
    form->addRow(tr("Quote currency"), quoteCombo_);
    form->addRow(tr("ORE key"), oreKeyLabel_);
    form->addRow(tr("Source name"), sourceNameEdit_);
    form->addRow(tr("Price source"), priceSourceRow);
    form->addRow(tr("Initial price"), priceSpin_);
    form->addRow(tr("Vintage source"), vintageSourceEdit_);
    form->addRow(tr("Vintage date"), vintageDateEdit_);
    form->addRow(QString(), enabledCheck_);
    layout->addLayout(form);
    layout->addStretch(1);

    tabWidget_->addTab(tab, tr("Instrument"));

    connect(baseCombo_, &QComboBox::currentTextChanged, this, &FxSpotRateEditor::onCurrencyChanged);
    connect(
        quoteCombo_, &QComboBox::currentTextChanged, this, &FxSpotRateEditor::onCurrencyChanged);
    connect(
        baseCombo_, &QComboBox::currentIndexChanged, this, &FxSpotRateEditor::onCurrencyChanged);
    connect(
        quoteCombo_, &QComboBox::currentIndexChanged, this, &FxSpotRateEditor::onCurrencyChanged);
    connect(sourceNameEdit_, &QLineEdit::textEdited, this, [this](const QString&) {
        userEditedSource_ = true;
    });
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
    for (const auto& e : kEngines)
        engineCombo_->addItem(tr(e.label), QString::fromLatin1(e.code));
    engineCombo_->setToolTip(
        tr("The price-process engine. Geometric uses log-returns (stays positive); "
           "arithmetic uses absolute price changes (symmetric, may go negative); "
           "Ornstein-Uhlenbeck reverts toward a long-run level (a single regime, not a "
           "mixture)."));
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
        const int seconds =
            fx_.ticks_per_hour > 0 ?
                std::max(1, static_cast<int>(std::lround(3600.0 / fx_.ticks_per_hour))) :
                1;
        secondsSpin_->setValue(seconds);
    }
    headerRow->addWidget(secondsSpin_);
    frequencyEchoLabel_ = new QLabel(tab);
    frequencyEchoLabel_->setStyleSheet("color: gray;");
    headerRow->addWidget(frequencyEchoLabel_);
    connect(secondsSpin_, &QSpinBox::valueChanged, this, [this](int) { recomputeFrequencyEcho(); });

    headerRow->addStretch(1);

    // Prominent segmented Simple/Advanced toggle (right).
    auto* simpleBtn = new QPushButton(tr("Simple"), tab);
    auto* advancedBtn = new QPushButton(tr("Advanced"), tab);
    const QColor accent = palette().color(QPalette::Highlight);
    const QColor accentText = palette().color(QPalette::HighlightedText);
    const QString segStyle =
        QStringLiteral("QPushButton { min-height: 30px; min-width: 110px; font-weight: bold; "
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
    engineWarningLabel_ = new QLabel(tab);
    engineWarningLabel_->setWordWrap(true);
    engineWarningLabel_->setStyleSheet("color:#d08020;");
    layout->addWidget(engineWarningLabel_);

    connect(
        engineCombo_, &QComboBox::currentIndexChanged, this, [this](int) { onEngineChanged(); });

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

    // RIGHT: compact PDF chart group, top-aligned. For single-regime engines
    // (e.g. "ou") the increment PDF doesn't apply, but the *steady-state price*
    // distribution does (closed-form: N(θ, σ/√(2κ))) — updateEngineUi() switches
    // the chart's domain rather than disabling it.
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

    updateEngineUi(); // initial header/tooltip/warning sync for the starting engine

    tabWidget_->addTab(tab, tr("Price behaviour"));
}

QWidget* FxSpotRateEditor::buildSimpleControls() {
    // "ou"'s Simple page has nothing in common with the GBM/arithmetic sliders
    // (single scalar params, not a drift/vol/jump mixture) — switch between two
    // independent pages rather than trying to make one widget cover both.
    simpleModeStack_ = new QStackedWidget(this);
    simpleModeStack_->addWidget(buildGbmSimpleControls()); // index 0
    simpleModeStack_->addWidget(buildOuSimpleControls());  // index 1
    return simpleModeStack_;
}

QWidget* FxSpotRateEditor::buildGbmSimpleControls() {
    auto* paramsBox = new QGroupBox(tr("Parameters"), this);
    paramsBox->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* paramsLayout = new QVBoxLayout(paramsBox);
    paramsLayout->setContentsMargins(12, 12, 12, 12);
    paramsLayout->setSpacing(8);

    const auto makeSlider =
        [&](const QString& title, const QString& tip, QSlider*& slider, QLabel*& valueLabel) {
            auto* header = new QHBoxLayout();
            auto* titleLabel = new QLabel(title, paramsBox);
            titleLabel->setToolTip(tip); // help shows on hover over the label
            header->addWidget(titleLabel);
            header->addStretch(1);
            valueLabel = new QLabel(paramsBox);
            valueLabel->setStyleSheet("color: gray;");
            header->addWidget(valueLabel);
            paramsLayout->addLayout(header);

            slider = new QSlider(Qt::Horizontal, paramsBox);
            slider->setRange(0, 100);
            slider->setToolTip(tip);
            paramsLayout->addWidget(slider);
        };

    makeSlider(tr("Global Trend Drift"),
               tr("Average direction per update (log-return drift)."),
               driftSlider_,
               driftValueLabel_);
    makeSlider(tr("Global Volatility"),
               tr("Typical size of each move (log-return volatility)."),
               volSlider_,
               volValueLabel_);
    makeSlider(tr("Jump Event Frequency"),
               tr("Approximate frequency of large jumps (modelled as a wide mixture "
                  "component — a GMM approximation, not a Poisson jump process)."),
               jumpSlider_,
               jumpValueLabel_);

    auto* note = new QLabel(
        tr("Switch to Advanced mode for direct GMM component control (μ, σ, w)."), paramsBox);
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

QWidget* FxSpotRateEditor::buildOuSimpleControls() {
    auto* box = new QGroupBox(tr("Stochastic Drivers"), this);
    box->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* layout = new QVBoxLayout(box);
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(8);

    // θ — read-only here; edited via the Initial Price field on the Instrument
    // tab, echoed so it's visible alongside the other two drivers.
    auto* thetaRow = new QHBoxLayout();
    auto* thetaTitle = new QLabel(tr("Long-Term Mean (θ)"), box);
    thetaTitle->setToolTip(tr("The level the price reverts toward. Set via the Initial Price "
                              "field on the Instrument tab."));
    thetaRow->addWidget(thetaTitle);
    thetaRow->addStretch(1);
    ouThetaLabel_ = new QLabel(box);
    ouThetaLabel_->setStyleSheet("color: gray;");
    thetaRow->addWidget(ouThetaLabel_);
    layout->addLayout(thetaRow);

    // κ — log-scale slider (see kappaSliderToValue/kappaValueToSlider) so one
    // control spans minutes- to months-long half-lives, paired with a spinbox
    // for exact entry, and a live half-life readout underneath.
    const QString kappaTip =
        tr("Mean reversion speed. Roughly, the price halves its distance from θ every "
           "ln(2)/κ updates. 0 = no reversion (driftless random walk).");
    auto* kappaTitle = new QLabel(tr("Mean Reversion Speed (κ)"), box);
    kappaTitle->setToolTip(kappaTip);
    layout->addWidget(kappaTitle);
    auto* kappaRow = new QHBoxLayout();
    kappaSlider_ = new QSlider(Qt::Horizontal, box);
    kappaSlider_->setRange(0, 100);
    kappaSlider_->setToolTip(kappaTip);
    kappaRow->addWidget(kappaSlider_, 1);
    kappaSpin_ = new QDoubleSpinBox(box);
    kappaSpin_->setRange(0.0, 10.0);
    kappaSpin_->setDecimals(6);
    kappaSpin_->setFixedWidth(110);
    kappaSpin_->setToolTip(kappaTip);
    kappaRow->addWidget(kappaSpin_);
    layout->addLayout(kappaRow);
    ouHalfLifeLabel_ = new QLabel(box);
    ouHalfLifeLabel_->setStyleSheet("color: gray;");
    layout->addWidget(ouHalfLifeLabel_);

    // σ — same slider+spinbox pattern, same range as the GBM Simple page's
    // volatility slider (kVolMin/kVolMax): both are a per-tick vol in %.
    const QString sigmaTip = tr("Size of each random nudge, before reversion pulls the price "
                                "back toward θ.");
    auto* sigmaTitle = new QLabel(tr("Volatility Coefficient (σ)"), box);
    sigmaTitle->setToolTip(sigmaTip);
    layout->addWidget(sigmaTitle);
    auto* sigmaRow = new QHBoxLayout();
    ouSigmaSlider_ = new QSlider(Qt::Horizontal, box);
    ouSigmaSlider_->setRange(0, 100);
    ouSigmaSlider_->setToolTip(sigmaTip);
    sigmaRow->addWidget(ouSigmaSlider_, 1);
    ouSigmaSpin_ = new QDoubleSpinBox(box);
    ouSigmaSpin_->setRange(0.0, 100.0);
    ouSigmaSpin_->setDecimals(3);
    ouSigmaSpin_->setSuffix(tr(" %"));
    ouSigmaSpin_->setFixedWidth(110);
    ouSigmaSpin_->setToolTip(sigmaTip);
    sigmaRow->addWidget(ouSigmaSpin_);
    layout->addLayout(sigmaRow);

    auto* note = new QLabel(
        tr("Switch to Advanced mode for the raw κ/σ table (e.g. to inspect provenance)."), box);
    note->setWordWrap(true);
    note->setStyleSheet("color: gray;");
    layout->addWidget(note);

    auto* resetRow = new QHBoxLayout();
    auto* resetBtn = new QPushButton(tr("Reset"), box);
    resetBtn->setToolTip(tr("Restore default parameters (15 min half-life, Normal volatility)."));
    connect(resetBtn, &QPushButton::clicked, this, &FxSpotRateEditor::onResetOuSimple);
    resetRow->addWidget(resetBtn);
    resetRow->addStretch(1);
    layout->addLayout(resetRow);
    layout->addStretch(1);

    // Slider <-> spinbox, kept in sync both ways without feedback loops; either
    // one changing rebuilds the model and refreshes charts.
    connect(kappaSlider_, &QSlider::valueChanged, this, [this](int v) {
        const QSignalBlocker blocker(kappaSpin_);
        kappaSpin_->setValue(kappaSliderToValue(v));
        if (syncing_)
            return;
        rebuildOuModelFromSimple();
        refreshCharts();
    });
    connect(kappaSpin_, &QDoubleSpinBox::valueChanged, this, [this](double v) {
        const QSignalBlocker blocker(kappaSlider_);
        kappaSlider_->setValue(kappaValueToSlider(v));
        if (syncing_)
            return;
        rebuildOuModelFromSimple();
        refreshCharts();
    });
    connect(ouSigmaSlider_, &QSlider::valueChanged, this, [this](int v) {
        const QSignalBlocker blocker(ouSigmaSpin_);
        ouSigmaSpin_->setValue(sliderToValue(v, kVolMin, kVolMax) * 100.0);
        if (syncing_)
            return;
        rebuildOuModelFromSimple();
        refreshCharts();
    });
    connect(ouSigmaSpin_, &QDoubleSpinBox::valueChanged, this, [this](double v) {
        const QSignalBlocker blocker(ouSigmaSlider_);
        ouSigmaSlider_->setValue(valueToSlider(v / 100.0, kVolMin, kVolMax));
        if (syncing_)
            return;
        rebuildOuModelFromSimple();
        refreshCharts();
    });

    return box;
}

QWidget* FxSpotRateEditor::buildAdvancedControls() {
    auto* compBox = new QGroupBox(tr("Gaussian Mixture Model Components"), this);
    compBox->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* compLayout = new QVBoxLayout(compBox);
    compLayout->setContentsMargins(12, 12, 12, 12);
    compLayout->setSpacing(8);

    // Short header labels — full explanations live in per-header tooltips
    // (updated per engine by updateEngineUi()) rather than permanent header
    // text, to keep this table from growing wider than the dialog can spare.
    componentTable_ = new QTableWidget(0, 7, compBox);
    componentTable_->setHorizontalHeaderLabels(
        {tr(""), tr("Name"), tr("Profile"), tr("μ"), tr("σ"), tr("w"), tr("")});
    if (auto* hdr = componentTable_->horizontalHeaderItem(ColColor))
        hdr->setToolTip(tr("Colour — matches this component's curve on the Return "
                           "distribution chart."));
    if (auto* hdr = componentTable_->horizontalHeaderItem(ColProfile))
        hdr->setToolTip(tr("A preset that fills in Volatility (σ)."));
    if (auto* hdr = componentTable_->horizontalHeaderItem(ColActions))
        hdr->setToolTip(tr("Remove this process."));
    // (A "Jump (planned)" column is intentionally omitted — not backed.)
    componentTable_->setMinimumWidth(480);
    componentTable_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    componentTable_->setShowGrid(false); // avoid cell-border + inner-widget "box in box"
    componentTable_->verticalHeader()->setVisible(false);
    componentTable_->verticalHeader()->setDefaultSectionSize(38); // roomier rows
    {
        // Name column stretches to fill; the rest size to their contents.
        auto* hdr = componentTable_->horizontalHeader();
        hdr->setStretchLastSection(false);
        hdr->setSectionResizeMode(ColName, QHeaderView::Stretch);
        for (int col : {ColColor, ColProfile, ColMean, ColStdev, ColWeight, ColActions})
            hdr->setSectionResizeMode(col, QHeaderView::ResizeToContents);
    }
    compLayout->addWidget(componentTable_);

    auto* compButtons = new QHBoxLayout();
    addComponentBtn_ = new QPushButton(tr("Add process"), compBox);
    addComponentBtn_->setToolTip(
        tr("Add another process — two or more processes form a regime mix."));
    connect(addComponentBtn_, &QPushButton::clicked, this, &FxSpotRateEditor::onAddComponentRow);
    compButtons->addWidget(addComponentBtn_);
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
    connect(watcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            self,
            [self, target, watcher]() {
                auto codes = watcher->result();
                watcher->deleteLater();
                if (!self || !target)
                    return;

                std::sort(codes.begin(), codes.end());
                self->knownCodes_ = codes;

                const QString preselect = (target == self->baseCombo_) ?
                                              QString::fromStdString(self->fx_.base_currency_code) :
                                              QString::fromStdString(self->fx_.quote_currency_code);

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
    return QString::fromStdString("ores.synthetic." + slug(feedName_) + "." + to_lower(base) + "." +
                                  to_lower(quote));
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
const QStringList kProfiles = {QStringLiteral("Custom"),
                               QStringLiteral("Flat"),
                               QStringLiteral("Calm"),
                               QStringLiteral("Normal"),
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

    // Colour indicator, matching the row's PDF curve colour on the Live Return
    // Distribution Preview chart (ReturnDistributionChart::componentColor()).
    auto* colorSwatch = new QLabel(componentTable_);
    colorSwatch->setFixedSize(16, 16);
    colorSwatch->setStyleSheet(QStringLiteral("background-color: %1; border-radius: 3px;")
                                   .arg(ReturnDistributionChart::componentColor(row).name()));
    auto* colorCell = new QWidget(componentTable_);
    auto* colorLayout = new QHBoxLayout(colorCell);
    colorLayout->setContentsMargins(4, 2, 4, 2);
    colorLayout->addWidget(colorSwatch, 0, Qt::AlignCenter);
    componentTable_->setCellWidget(row, ColColor, colorCell);

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
    profileCombo->view()->setMinimumWidth(profileCombo->view()->sizeHintForColumn(0) + 16);
    profileCombo->setStyleSheet(QStringLiteral("QComboBox { border: none; background: "
                                               "transparent; }"));

    auto* meanSpin = new QDoubleSpinBox(componentTable_);
    meanSpin->setRange(-100.0, 100.0);
    meanSpin->setDecimals(3);
    meanSpin->setSuffix(tr(" %"));
    meanSpin->setValue(c.mean * 100.0);
    meanSpin->setFrame(false);
    meanSpin->setStyleSheet(flatEdit);
    // Explanation lives on the column header tooltip (ColMean), which updates per
    // engine — a per-row static tooltip here would go stale for e.g. "ou".

    auto* stdevSpin = new QDoubleSpinBox(componentTable_);
    stdevSpin->setRange(0.0, 100.0);
    stdevSpin->setDecimals(3);
    stdevSpin->setSuffix(tr(" %"));
    stdevSpin->setValue(c.stdev * 100.0);
    stdevSpin->setFrame(false);
    stdevSpin->setStyleSheet(flatEdit); // see ColStdev header tooltip

    auto* weightSpin = new QDoubleSpinBox(componentTable_);
    weightSpin->setRange(0.0, 1e6);
    weightSpin->setDecimals(3);
    weightSpin->setValue(c.weight);
    weightSpin->setFrame(false);
    weightSpin->setStyleSheet(flatEdit); // see ColWeight header tooltip

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
    connect(
        profileCombo, &QComboBox::currentTextChanged, this, [this, profileCombo](const QString& p) {
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
        updateComponentColors(); // remaining rows shifted up — repaint swatches
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

bool FxSpotRateEditor::currentEngineSupportsMixing() const {
    const auto* info = find_engine(currentEngine());
    return info ? info->supportsMixing : true; // unknown engine: don't block on it
}

QString FxSpotRateEditor::incrementNoun() const {
    return currentEngine() == "arithmetic" ? tr("price change") : tr("log-return");
}

void FxSpotRateEditor::onEngineChanged() {
    if (engineCombo_)
        fx_.process_type = currentEngine();
    updateEngineUi();
    // Engine also determines which distribution the Return Distribution chart
    // plots (increment PDF, or "ou"'s steady-state price distribution).
    refreshCharts();
}

void FxSpotRateEditor::updateEngineUi() {
    // Generic, capability-driven gating (any engine with supportsMixing = false
    // behaves this way, not just "ou" specifically). Called both on every engine
    // change AND once at the end of buildBehaviourTab() during construction.
    const bool mixing = currentEngineSupportsMixing();
    if (!mixing && components_.size() > 1) {
        // Single-regime process — collapse to the first row so its reused
        // fields have one unambiguous value.
        components_.resize(1);
    }
    // The warning banner's wording is inherently engine-specific — a boolean
    // can't express *how* a single-regime engine's fields are repurposed, so
    // this part stays keyed on the engine code, unlike the gating above.
    const auto engine = currentEngine();
    const bool ou = engine == "ou";
    const bool arithmetic = engine == "arithmetic";

    // κ = 1.0 is the generic single-row default weight (a full mixture share),
    // but reused as OU's reversion speed it reverts within ~1 tick — the price
    // sits in a band so tight it looks "stuck" rather than visibly mean-
    // reverting. Rescale it to a slower, more legible default the first time a
    // *new* config switches to "ou", without touching a loaded record's real κ.
    if (ou && isNew_ && !components_.empty() && components_.front().weight == 1.0)
        components_.front().weight = kappaFromHalfLifeMinutes(15.0);

    // Always resync both the table and "ou"'s dedicated Simple page from the
    // model — cheap, and guarantees whichever page becomes visible (either via
    // this engine change or a later Simple/Advanced click) is never stale.
    if (componentTable_) {
        syncing_ = true;
        syncAdvancedFromModel();
        syncing_ = false;
        updateComponentColors();
    }
    if (simpleModeStack_) {
        simpleModeStack_->setCurrentIndex(ou ? 1 : 0);
        if (ou) {
            syncing_ = true;
            syncOuSimpleFromModel();
            syncing_ = false;
        }
    }

    // Only arithmetic gets a warning: its negative-price possibility is a real
    // surprise nothing else in the UI communicates. OU's single-regime nature
    // is now self-evident from its dedicated Simple page and the Advanced
    // table's header/row tooltips, so it doesn't need a persistent banner.
    if (engineWarningLabel_) {
        if (arithmetic) {
            engineWarningLabel_->setText(
                tr("⚠ Absolute price increments — may go negative. Hover for details."));
            engineWarningLabel_->setToolTip(
                tr("Arithmetic engine: μ and σ are absolute price increments (not "
                   "%/log-returns), and the price can go negative or zero — unrealistic for an "
                   "FX rate. Intended for testing the process abstraction."));
        }
        engineWarningLabel_->setVisible(arithmetic);
    }

    // The Return Distribution chart's domain (increment PDF vs. steady-state
    // price) is set from refreshCharts(), which also supplies its components —
    // no separate handling needed here.

    if (addComponentBtn_)
        addComponentBtn_->setEnabled(mixing);

    if (componentTable_) {
        if (auto* meanHdr = componentTable_->horizontalHeaderItem(ColMean))
            meanHdr->setToolTip(
                ou ? tr("Unused by Ornstein-Uhlenbeck.") :
                     tr("Drift (μ) — average %1 per update (%).").arg(incrementNoun()));
        if (auto* stdevHdr = componentTable_->horizontalHeaderItem(ColStdev))
            stdevHdr->setToolTip(
                ou ? tr("σ — Ornstein-Uhlenbeck volatility.") :
                     tr("Volatility (σ) of the %1 per update (%).").arg(incrementNoun()));
        if (auto* weightHdr = componentTable_->horizontalHeaderItem(ColWeight)) {
            // The header text itself, not just its tooltip: "w" reads as a weight/
            // share, which is misleading once this column holds κ instead.
            weightHdr->setText(ou ? tr("κ") : tr("w"));
            weightHdr->setToolTip(
                ou ? tr("κ — Ornstein-Uhlenbeck reversion speed (0 = driftless random walk).") :
                     tr("Relative share when blending processes (normalised on save)."));
        }

        // Detailed per-row tooltip on hover, in addition to the column-header ones
        // above — explains what this specific component's three numbers mean in
        // the current engine's terms, without having to reach for the headers.
        const QString rowTip = componentTooltip(ou, arithmetic);
        for (int r = 0; r < componentTable_->rowCount(); ++r) {
            for (int col : {ColName, ColMean, ColStdev, ColWeight}) {
                if (auto* w = componentTable_->cellWidget(r, col))
                    w->setToolTip(rowTip);
            }
        }
    }
}

double FxSpotRateEditor::secondsPerTick() const {
    return secondsSpin_ ? std::max(1, secondsSpin_->value()) : 1.0;
}

double FxSpotRateEditor::halfLifeMinutesFromKappa(double kappa) const {
    if (kappa <= 0.0)
        return 0.0; // 0 = driftless random walk, shown as 0 (infinite half-life)
    return (std::log(2.0) / kappa) * secondsPerTick() / 60.0;
}

double FxSpotRateEditor::kappaFromHalfLifeMinutes(double halfLifeMinutes) const {
    if (halfLifeMinutes <= 0.0)
        return 0.0;
    return std::log(2.0) / (halfLifeMinutes * 60.0 / secondsPerTick());
}

QString FxSpotRateEditor::componentTooltip(bool ou, bool arithmetic) const {
    if (ou) {
        return tr("<b>Ornstein-Uhlenbeck component</b><br>"
                  "θ (long-run mean): the <i>Initial Price</i> field above — the level the "
                  "price reverts toward.<br>"
                  "κ (reversion speed, this row's <i>Weight</i>): how fast the price is pulled "
                  "back to θ. Roughly, the price halves its distance from θ every "
                  "ln(2)/κ updates — κ = 1 reverts almost immediately (tight, \"stuck\"-looking "
                  "band); κ = 0.02–0.05 wanders visibly before reverting.<br>"
                  "σ (volatility, this row's <i>σ</i>): the size of each random nudge, before "
                  "reversion pulls it back. The band the price settles into is roughly "
                  "σ/√(2κ) wide — small κ and larger σ together give a wider, more visible "
                  "range.<br>"
                  "μ (Drift) is unused by this engine.");
    }
    if (arithmetic) {
        return tr("<b>Arithmetic component</b><br>"
                  "μ (Drift): average absolute price change per update.<br>"
                  "σ (Volatility): standard deviation of the absolute price change per "
                  "update.<br>"
                  "Weight: this component's relative share when blending with other "
                  "components (normalised to sum to 1 on save).<br>"
                  "Unlike Geometric, these are absolute price units, not %/log-returns — the "
                  "price can go negative.");
    }
    return tr("<b>Geometric component</b><br>"
              "μ (Drift): average log-return per update (%) — the trend.<br>"
              "σ (Volatility): standard deviation of the log-return per update (%) — the "
              "noise.<br>"
              "Weight: this component's relative share when blending with other components "
              "(normalised to sum to 1 on save); e.g. a small-weight, high-σ component "
              "approximates a jump regime.");
}

void FxSpotRateEditor::onAddComponentRow() {
    addTableRow(ModelComponent{{}, "Process", 0.0, v0 * 2, 1.0});
    rebuildModelFromAdvanced();
    updateRemoveButtonsEnabled();
    updateEngineUi(); // stamp the new row's cells with the current engine's tooltip
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

void FxSpotRateEditor::updateComponentColors() {
    for (int r = 0; r < componentTable_->rowCount(); ++r) {
        if (auto* cell = componentTable_->cellWidget(r, ColColor)) {
            if (auto* swatch = cell->findChild<QLabel*>())
                swatch->setStyleSheet(QStringLiteral("background-color: %1; border-radius: 3px;")
                                          .arg(ReturnDistributionChart::componentColor(r).name()));
        }
    }
}

void FxSpotRateEditor::onResetSimple() {
    syncing_ = true;
    driftSlider_->setValue(valueToSlider(0.0, kDriftMin, kDriftMax)); // no trend
    volSlider_->setValue(valueToSlider(v0 * 2, kVolMin, kVolMax));    // Normal volatility
    jumpSlider_->setValue(valueToSlider(0.0, kJumpMin, kJumpMax));    // no jumps
    syncing_ = false;
    rebuildModelFromSimple();
    refreshCharts();
}

QString FxSpotRateEditor::ouHalfLifeText(double kappa) const {
    if (kappa <= 0.0)
        return tr("κ = 0 — no reversion (driftless random walk)");
    const double minutes = halfLifeMinutesFromKappa(kappa);
    return tr("Calculated Half-Life t½: %1 min (%2 days)")
        .arg(minutes, 0, 'f', 3)
        .arg(minutes / (24.0 * 60.0), 0, 'f', 1);
}

void FxSpotRateEditor::syncOuSimpleFromModel() {
    if (priceSpin_)
        ouThetaLabel_->setText(tr("%1").arg(priceSpin_->value(), 0, 'f', 5));
    const double kappa = components_.empty() ? 0.0 : components_.front().weight;
    const double sigma = components_.empty() ? 0.0 : components_.front().stdev;

    syncing_ = true;
    kappaSpin_->setValue(kappa);
    kappaSlider_->setValue(kappaValueToSlider(kappa));
    ouSigmaSpin_->setValue(sigma * 100.0);
    ouSigmaSlider_->setValue(valueToSlider(sigma, kVolMin, kVolMax));
    syncing_ = false;

    ouHalfLifeLabel_->setText(ouHalfLifeText(kappa));
}

void FxSpotRateEditor::rebuildOuModelFromSimple() {
    const double kappa = kappaSpin_->value();
    const double sigma = ouSigmaSpin_->value() / 100.0;

    ouHalfLifeLabel_->setText(ouHalfLifeText(kappa));

    // Preserve the existing single component's id so this updates in place.
    std::string id;
    if (!components_.empty())
        id = components_.front().id;
    components_ = {ModelComponent{id, "Primary process", 0.0, sigma, kappa}};
}

void FxSpotRateEditor::onResetOuSimple() {
    components_ = {ModelComponent{components_.empty() ? std::string() : components_.front().id,
                                  "Primary process",
                                  0.0,
                                  v0 * 2,
                                  kappaFromHalfLifeMinutes(15.0)}};
    syncing_ = true;
    syncOuSimpleFromModel();
    syncing_ = false;
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
    updateWeightSumLabel();
}

void FxSpotRateEditor::rebuildModelFromAdvanced() {
    std::vector<ModelComponent> next;
    for (int r = 0; r < componentTable_->rowCount(); ++r) {
        auto* nameEdit = qobject_cast<QLineEdit*>(componentTable_->cellWidget(r, ColName));
        auto* meanSpin = qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColMean));
        auto* stdevSpin = qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColStdev));
        auto* weightSpin = qobject_cast<QDoubleSpinBox*>(componentTable_->cellWidget(r, ColWeight));
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
    updateWeightSumLabel();
}

void FxSpotRateEditor::updateWeightSumLabel() {
    if (!currentEngineSupportsMixing()) {
        // Wording is OU-specific for now (the only non-mixing engine); a future
        // second one will need its own case here, same as the warning banner.
        const double kappa = components_.empty() ? 0.0 : components_.front().weight;
        weightSumLabel_->setText(tr("κ (reversion speed) = %1").arg(kappa, 0, 'f', 3));
        return;
    }
    double sum = 0.0;
    for (const auto& c : components_)
        sum += c.weight;
    weightSumLabel_->setText(tr("Weight Sum = %1 (normalised on save)").arg(sum, 0, 'f', 3));
}

void FxSpotRateEditor::syncSimpleFromModel() {
    if (currentEngine() == "ou") {
        syncOuSimpleFromModel();
        return;
    }
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
    if (currentEngine() == "ou") {
        rebuildOuModelFromSimple();
        return;
    }
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
    next.push_back(ModelComponent{primaryId, "Primary process", drift, vol, 1.0 - jumpFraction});
    // Jump regime: a wider component (σ ≈ 6× the base volatility). It scales with
    // vol with no baseline floor, so at vol≈0 the jump collapses to ~0 width and
    // cannot dominate the distribution.
    if (jumpFraction > 0.0) {
        next.push_back(
            ModelComponent{jumpId, "Jump regime (approx.)", 0.0, vol * 6.0, jumpFraction});
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
    const std::string engine = currentEngine();
    const double price = priceSpin_ ? priceSpin_->value() : fx_.gmm_initial_price;

    const bool ou = engine == "ou";
    if (distChart_)
        distChart_->setDomain(ou ? ReturnDistributionChart::Domain::Price :
                                   ReturnDistributionChart::Domain::Return);

    if (!currentEngineSupportsMixing()) {
        // Single-regime path — wording/remapping below is OU-specific for now (the
        // only such engine); a future second one will need its own case here.
        // Scalar params (κ, σ) reused from Weight/Volatility — not a mixture, so no
        // normalisation. θ (long-run mean) is the Initial Price.
        if (!components_.empty()) {
            const double kappa = components_.front().weight;
            const double sigma = components_.front().stdev;
            pathComps.push_back({0.0, sigma, kappa});
            // Steady-state price distribution is closed-form for OU:
            // N(θ, σ/√(2κ)) — undefined (no stationary distribution) at κ = 0.
            if (ou && kappa > 0.0)
                distComps.push_back({price, sigma / std::sqrt(2.0 * kappa), 1.0});
        }
    } else {
        double sum = 0.0;
        for (const auto& c : components_)
            sum += c.weight;
        for (const auto& c : components_) {
            const double w = sum > 0.0 ? c.weight / sum : c.weight;
            distComps.push_back({c.mean, c.stdev, w});
            pathComps.push_back({c.mean, c.stdev, w});
        }
    }

    if (distChart_)
        distChart_->setComponents(distComps); // empty for κ=0 "ou" — no stationary distribution
    if (pathsChart_) {
        pathsChart_->setComponents(pathComps);
        pathsChart_->setInitialPrice(price);
        pathsChart_->setProcessType(engine);
        // Mean reversion isn't obvious from a noisy path alone — draw θ as a
        // reference line so it's visible what the price is reverting toward.
        pathsChart_->setReferenceLevel(ou ? std::make_optional(price) : std::nullopt);
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
        QMessageBox::warning(
            this, tr("Disconnected"), tr("Cannot save while disconnected from the server."));
        return;
    }

    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();

    if (base.empty() || quote.empty()) {
        QMessageBox::warning(
            this, tr("Incomplete"), tr("Both base and quote currencies must be set."));
        return;
    }
    if (base == quote) {
        QMessageBox::warning(
            this, tr("Invalid pair"), tr("Base and quote currencies must differ."));
        return;
    }
    const auto isKnown = [this](const std::string& c) {
        return std::find(knownCodes_.begin(), knownCodes_.end(), c) != knownCodes_.end();
    };
    if (!isKnown(base) || !isKnown(quote)) {
        QMessageBox::warning(
            this, tr("Unknown currency"), tr("Both base and quote must be valid currency codes."));
        return;
    }

    const bool vintageMode = priceSourceGroup_->checkedId() == 1;
    if (vintageMode && (vintageSourceEdit_->text().trimmed().isEmpty() ||
                       vintageDateEdit_->text().trimmed().isEmpty())) {
        QMessageBox::warning(this,
                             tr("Incomplete"),
                             tr("Vintage source and date are both required when the price source "
                                "is \"From vintage data\"."));
        return;
    }

    const auto crOpType = isNew_ ? ChangeReasonDialog::OperationType::Create :
                                   ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, true, isNew_ ? "system" : "common");
    if (!crSel)
        return;

    // Make sure the model reflects whichever surface is currently active.
    if (modeGroup_ && modeGroup_->checkedId() == 1)
        rebuildModelFromAdvanced();
    else
        rebuildModelFromSimple();

    // Ask the engine itself whether these parameters are valid — the same rules
    // process_factory enforces server-side (ores.synthetic.api::domain, shared,
    // not duplicated UI-side logic).
    {
        std::vector<double> means, stdevs, weights;
        for (const auto& mc : components_) {
            means.push_back(mc.mean);
            stdevs.push_back(mc.stdev);
            weights.push_back(mc.weight);
        }
        const auto validation = synthetic::domain::validate_process_parameters(
            currentEngine(), means, stdevs, weights, priceSpin_->value());
        if (!validation.valid) {
            QMessageBox::warning(
                this, tr("Invalid parameters"), QString::fromStdString(validation.message));
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
    if (vintageMode) {
        fx.price_source = "vintage";
        fx.gmm_initial_price = 0.0;
        fx.vintage_source = vintageSourceEdit_->text().trimmed().toStdString();
        fx.vintage_date = vintageDateEdit_->text().trimmed().toStdString();
    } else {
        fx.price_source = "fixed";
        fx.gmm_initial_price = priceSpin_->value();
        fx.vintage_source.clear();
        fx.vintage_date.clear();
    }
    fx.ticks_per_hour =
        std::max(1, static_cast<int>(std::lround(3600.0 / std::max(1, secondsSpin_->value()))));
    fx.enabled = enabledCheck_->isChecked();
    fx.party_id = clientManager_->currentPartyId();
    fx.modified_by = username_.toStdString();
    fx.change_reason_code = crSel->reason_code;
    fx.change_commentary =
        crSel->commentary.empty() ? "Authored via Market Simulator" : crSel->commentary;
    fx.version = 0;

    // Build the component stack, normalising weights to sum 1. Skipped for
    // single-regime engines (e.g. "ou"): their one row's Weight field is a
    // scalar parameter (κ), not a mixture share.
    const bool mixing = currentEngineSupportsMixing();
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
        c.weight = (mixing && total > 0.0) ? mc.weight / total : mc.weight;
        c.modified_by = username_.toStdString();
        namespace reason = ores::dq::domain::change_reason_constants::codes;
        c.change_reason_code =
            rowIsNew ? std::string(reason::new_record) : std::string(reason::non_material_update);
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

        // Delete stale components *before* writing new ones: the unique index on
        // (party_id, fx_spot_config_id, component_index) only covers still-active
        // rows, so a fresh component reusing an index still held by a to-be-deleted
        // row (e.g. collapsing to a single-regime engine) would otherwise hit that
        // constraint on INSERT.
        if (!toDelete.empty()) {
            auto dResp =
                cm->process_authenticated_request(m::delete_gmm_component_request{.ids = toDelete});
            if (!dResp)
                return {false, QString::fromStdString(dResp.error())};
            if (!dResp->success)
                return {false, QString::fromStdString(dResp->message)};
        }

        for (const auto& c : comps) {
            auto cResp = cm->process_authenticated_request(m::save_gmm_component_request::from(c));
            if (!cResp)
                return {false, QString::fromStdString(cResp.error())};
            if (!cResp->success)
                return {false, QString::fromStdString(cResp->message)};
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
