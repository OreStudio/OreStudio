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
#include "ores.qt/IrCurveEditor.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.qt/CurveShapePreviewChart.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/OreCurrencyComboBox.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/SampleShortRatePathsChart.hpp"
#include "ores.refdata.api/messaging/floating_index_type_protocol.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_process_parameter_value_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include <QButtonGroup>
#include <QComboBox>
#include <QCompleter>
#include <QDialog>
#include <QDialogButtonBox>
#include <QDoubleSpinBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QListWidget>
#include <QMessageBox>
#include <QPalette>
#include <QPointer>
#include <QPushButton>
#include <QRadioButton>
#include <QSlider>
#include <QSizePolicy>
#include <QSplitter>
#include <QStackedWidget>
#include <QTableWidget>
#include <QTimer>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <format>
#include <limits>
#include <map>
#include <set>
#include <tuple>

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr int ColStart = 0;
constexpr int ColEnd = 1;
constexpr int ColInstrument = 2;

struct EngineInfo {
    const char* code;  // matches process_type / process_factory's dispatch key
    const char* label; // combo box display text (wrapped in tr() at use site)
};
constexpr EngineInfo kEngines[] = {
    {"VASICEK", QT_TR_NOOP("Vasicek")},
    {"COX_INGERSOLL_ROSS", QT_TR_NOOP("Cox-Ingersoll-Ross")},
    {"HULL_WHITE", QT_TR_NOOP("Hull-White")},
    {"TWO_FACTOR_GAUSSIAN", QT_TR_NOOP("Two-Factor Gaussian")},
    {"BLACK_KARASINSKI", QT_TR_NOOP("Black-Karasinski")},
};

// indexNameCombo_ still lists the floating_index_type suffix shape this editor has
// always fetched (e.g. "SOFR", "LIBOR-3M") -- split/rejoin at the oresmd
// index_family/tenor boundary here rather than restructuring the combo/fetch
// itself, since oresmd's index_family enum values are exactly those suffixes'
// lower-cased family segment.
std::pair<std::string, std::string> splitIndexFamilyAndTenor(const std::string& suffix) {
    const auto dash = suffix.find('-');
    std::string family = dash == std::string::npos ? suffix : suffix.substr(0, dash);
    std::string tenor = dash == std::string::npos ? std::string() : suffix.substr(dash + 1);
    std::transform(family.begin(), family.end(), family.begin(), [](unsigned char c) {
        return std::tolower(c);
    });
    return {family, tenor};
}

QString joinIndexFamilyAndTenor(const std::string& index_family, const std::string& tenor) {
    auto family = index_family;
    std::transform(family.begin(), family.end(), family.begin(), [](unsigned char c) {
        return std::toupper(c);
    });
    if (tenor.empty())
        return QString::fromStdString(family);
    return QString::fromStdString(family + "-" + tenor);
}

// Simple-mode slider <-> double conversion: sliders are integer-valued while parameters are real
// numbers, so scale by 1e6 both ways (matching the spins' 6-decimal precision). The scale can
// overflow int for unbounded parameters near the ±1e6 spin fallback, so conversions clamp to the
// slider's representable range -- extremes stay the preserve of the precise spins.
constexpr int kSimpleSliderScale = 1'000'000;

int doubleToSliderValue(double v) {
    return static_cast<int>(std::clamp(
        std::llround(v * kSimpleSliderScale),
        static_cast<long long>(std::numeric_limits<int>::min()),
        static_cast<long long>(std::numeric_limits<int>::max())));
}

double sliderValueToDouble(int sv) {
    return static_cast<double>(sv) / kSimpleSliderScale;
}

} // namespace

ProvenanceWidget* IrCurveEditor::provenanceWidget() const {
    return provenanceWidget_;
}

IrCurveEditor::IrCurveEditor(ClientManager* cm,
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

    ir_.id = boost::uuids::random_generator()();
    ir_.config_id = parentFeedId;
    ir_.process_type = "VASICEK";
    // No parameter defaults here -- they come from the yield_curve_process_parameter_definition
    // catalogue, fetched (with the value rows, edit mode) by populateParameterRows() when the
    // Process tab is built. Values are plain, real annualised numbers matching the seeded
    // Barclays curves' magnitude; the mapping layer threads dt (1/365 for this feed's
    // day-per-tick convention) through separately.
    ir_.ticks_per_hour = 3600;
    ir_.enabled = true;
    ir_.fixed_leg_payment_frequency_code = "Annual";

    BOOST_LOG_SEV(lg(), info) << "Opening IR curve editor for a new curve "
                              << boost::uuids::to_string(ir_.id) << " under feed "
                              << boost::uuids::to_string(parentFeedId) << ".";

    // Seed a sensible default 3-entry template (short deposit, FRA, swap).
    // Instrument codes are the refdata catalogue's mnemonics (DEPO/FRA/IRS), not
    // display names -- the curve preview resolves them strictly by code.
    entries_.push_back(TemplateRow{{}, "SPOT", "3M", "DEPO"});
    entries_.push_back(TemplateRow{{}, "3M", "6M", "FRA"});
    entries_.push_back(TemplateRow{{}, "SPOT", "2Y", "IRS"});

    buildUi();
    syncTableFromModel();
    refreshCharts();
    setProvenanceEnabled(false);
}

IrCurveEditor::IrCurveEditor(ClientManager* cm,
                             ImageCache* imageCache,
                             ChangeReasonCache* crCache,
                             const QString& username,
                             const synthetic::domain::ir_curve_generation_config& existing,
                             const QString& feedName,
                             const std::vector<synthetic::domain::ir_curve_template_entry>& entries,
                             QWidget* parent)
    : DetailDialogBase(parent)
    , clientManager_(cm)
    , imageCache_(imageCache)
    , username_(username)
    , feedName_(feedName)
    , isNew_(false)
    , userEditedSource_(true) // existing source name is authoritative; don't overwrite
    , ir_(existing) {

    setChangeReasonCache(crCache);

    BOOST_LOG_SEV(lg(), info) << "Opening IR curve editor editing curve "
                              << boost::uuids::to_string(ir_.id) << " with " << entries.size()
                              << " Curve Template entries.";

    auto sorted = entries;
    std::sort(sorted.begin(), sorted.end(), [](const auto& a, const auto& b) {
        return a.sequence_index < b.sequence_index;
    });
    for (const auto& e : sorted) {
        originalEntryIds_.push_back(boost::uuids::to_string(e.id));
        entries_.push_back(TemplateRow{boost::uuids::to_string(e.id),
                                       e.start_tenor_code,
                                       e.end_tenor_code,
                                       e.instrument_code});
    }

    buildUi();
    syncTableFromModel();
    refreshCharts();

    populateProvenance(ir_.version,
                       ir_.modified_by,
                       ir_.performed_by,
                       ir_.recorded_at,
                       ir_.change_reason_code,
                       ir_.change_commentary);
    setProvenanceEnabled(true);
}

void IrCurveEditor::buildUi() {
    setWindowTitle(isNew_ ? tr("New IR Curve") : tr("Edit IR Curve"));

    auto* layout = new QVBoxLayout(this);
    tabWidget_ = new QTabWidget(this);
    layout->addWidget(tabWidget_, 1);

    buildInstrumentTab();
    buildProcessTab();
    buildCurveTemplateTab();

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
    connect(buttons, &QDialogButtonBox::accepted, this, &IrCurveEditor::onSaveClicked);
    connect(buttons, &QDialogButtonBox::rejected, this, &IrCurveEditor::onCloseClicked);
}

void IrCurveEditor::buildInstrumentTab() {
    auto* tab = new QWidget(this);
    auto* outer = new QVBoxLayout(tab);

    auto* intro = new QLabel(
        tr("Pick the currency and floating-rate index this curve represents. The source name "
           "(the NATS subject/observation source ticks publish under) is derived from these."),
        tab);
    intro->setWordWrap(true);
    intro->setStyleSheet("color: gray; font-style: italic;");
    outer->addWidget(intro);

    // Matches every codegen detail dialog's framing (e.g. Portfolio's "Basic Information",
    // Book's field groups) -- a titled QGroupBox around the form, not the form bare on the tab.
    auto* identityBox = new QGroupBox(tr("Curve Identity"), tab);
    auto* form = new QFormLayout(identityBox);

    // Same ~180-row ISO currency list and "pick 1 of many quickly" use case as FX's own
    // base/quote combos -- editable + completer, same as FxSpotRateEditor::buildInstrumentTab().
    currencyCombo_ = new OreCurrencyComboBox(identityBox);
    currencyCombo_->setEditable(true);
    currencyCombo_->setInsertPolicy(QComboBox::NoInsert);
    currencyCombo_->completer()->setCompletionMode(QCompleter::PopupCompletion);
    currencyCombo_->completer()->setFilterMode(Qt::MatchContains);
    currencyCombo_->completer()->setCaseSensitivity(Qt::CaseInsensitive);
    form->addRow(tr("Currency"), currencyCombo_);

    // Unlike the currency combo, this list is short (post-filter: 1-3 rows per currency, not
    // ~180) -- a completer would solve a problem that doesn't exist here.
    indexNameCombo_ = new QComboBox(identityBox);
    indexNameCombo_->setEditable(false);
    indexNameCombo_->setInsertPolicy(QComboBox::NoInsert);
    form->addRow(tr("Index name"), indexNameCombo_);

    // discount/projection/self_discounting -- lets a discount curve and a projection curve for
    // the same currency+index+tenor coexist (feed_controller's collision check keys on
    // (qualifier, role), not qualifier alone). Defaults to self_discounting, the shape every
    // pre-existing synthetic curve assumes.
    roleCombo_ = new QComboBox(identityBox);
    roleCombo_->setEditable(false);
    roleCombo_->setInsertPolicy(QComboBox::NoInsert);
    roleCombo_->addItem(tr("Self-discounting"), QString("self_discounting"));
    roleCombo_->addItem(tr("Discount"), QString("discount"));
    roleCombo_->addItem(tr("Projection"), QString("projection"));
    if (const int idx = roleCombo_->findData(QString::fromStdString(ir_.role)); idx >= 0)
        roleCombo_->setCurrentIndex(idx);
    form->addRow(tr("Role"), roleCombo_);

    // Mirrors FxSpotRateEditor::buildInstrumentTab()'s ORE key label -- shows the exact key
    // SyntheticBindingDialog will bind this config under (currency_code + "/" + stripped index
    // name), so the tester doesn't have to work it out by hand.
    oreKeyLabel_ = new QLabel(identityBox);
    oreKeyLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("ORE key"), oreKeyLabel_);

    fixedLegFrequencyCombo_ = new QComboBox(identityBox);
    fixedLegFrequencyCombo_->setInsertPolicy(QComboBox::NoInsert);
    form->addRow(tr("Fixed leg frequency"), fixedLegFrequencyCombo_);

    enabledCheck_ = new QCheckBox(tr("Enabled"), identityBox);
    enabledCheck_->setChecked(ir_.enabled);
    form->addRow(QString(), enabledCheck_);

    secondsSpin_ = new QSpinBox(identityBox);
    secondsSpin_->setRange(1, 3600);
    secondsSpin_->setSuffix(tr(" s"));
    secondsSpin_->setValue(
        ir_.ticks_per_hour > 0 ?
            std::max(1, static_cast<int>(std::lround(3600.0 / ir_.ticks_per_hour))) :
            1);
    form->addRow(tr("New tick every"), secondsSpin_);

    sourceNameEdit_ = new QLineEdit(identityBox);
    sourceNameEdit_->setText(QString::fromStdString(ir_.source_name));
    sourceNameEdit_->setToolTip(
        tr("The NATS subject suffix and market_observation.source ticks publish under. A default "
           "is derived from the collection, currency, and index; editable."));
    form->addRow(tr("Source"), sourceNameEdit_);

    outer->addWidget(identityBox);

    // Price source: mirrors FxSpotRateEditor::buildInstrumentTab()'s own layout (two radio-headed
    // groups, checking one disables the other) -- the field this gates (r0) lives on the Process
    // tab rather than here, unlike FX's Initial price, so vintage mode instead disables that
    // tab's initial-rate parameter row's spin box (see updatePriceSourceEnablement()).
    auto* priceSourceIntro =
        new QLabel(tr("Where this curve's starting rate (r0) comes from — pick one."), tab);
    priceSourceIntro->setStyleSheet("color: gray; font-style: italic;");
    outer->addWidget(priceSourceIntro);

    fixedRadio_ = new QRadioButton(tr("Fixed"), tab);
    vintageRadio_ = new QRadioButton(tr("From vintage data"), tab);
    priceSourceGroup_ = new QButtonGroup(this);
    priceSourceGroup_->setExclusive(true);
    priceSourceGroup_->addButton(fixedRadio_, 0);
    priceSourceGroup_->addButton(vintageRadio_, 1);
    outer->addWidget(fixedRadio_);
    auto* fixedNote = new QLabel(tr("r0 is set on the Process tab, used as entered."), tab);
    fixedNote->setContentsMargins(20, 0, 0, 8);
    fixedNote->setStyleSheet("color: gray;");
    outer->addWidget(fixedNote);

    outer->addWidget(vintageRadio_);
    auto* vintageForm = new QFormLayout();
    vintageForm->setContentsMargins(20, 0, 0, 8);
    vintageSourceEdit_ = new QLineEdit(tab);
    vintageSourceEdit_->setText(QString::fromStdString(ir_.vintage_source));
    vintageSourceEdit_->setPlaceholderText(tr("e.g. ore.samples.2016-02-05"));
    vintageDateEdit_ = new QLineEdit(tab);
    vintageDateEdit_->setText(QString::fromStdString(ir_.vintage_date));
    vintageDateEdit_->setPlaceholderText(tr("YYYY-MM-DD"));
    vintageForm->addRow(tr("Vintage source"), vintageSourceEdit_);
    vintageForm->addRow(tr("Vintage date"), vintageDateEdit_);
    browseVintageButton_ = new QPushButton(tr("Browse available…"), tab);
    browseVintageButton_->setToolTip(
        tr("List the (source, date) vintages actually imported for this currency/index, "
           "instead of typing them blind."));
    browseVintageButton_->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
    vintageForm->addRow(QString(), browseVintageButton_);
    outer->addLayout(vintageForm);
    connect(
        browseVintageButton_, &QPushButton::clicked, this, &IrCurveEditor::onBrowseVintageClicked);

    outer->addStretch(1);

    const bool vintageMode = ir_.price_source == "vintage";
    (vintageMode ? vintageRadio_ : fixedRadio_)->setChecked(true);
    connect(priceSourceGroup_, &QButtonGroup::idClicked, this, [this](int id) {
        BOOST_LOG_SEV(lg(), debug) << "priceSourceGroup_ idClicked: id=" << id;
        updatePriceSourceEnablement();
    });
    // buildProcessTab() runs after this method returns, so parameterTable_/initialRateSpin_ don't
    // exist yet (and are only populated once populateParameterRows()'s async fetch lands) --
    // rebuildParameterTable() re-runs this after every population, so no deferred call needed
    // here.
    updatePriceSourceEnablement();

    connect(currencyCombo_, &QComboBox::currentTextChanged, this, [this](const QString&) {
        populateIndexNameCombo();
        recomputeOreKey();
        recomputeDefaultSourceName();
    });
    connect(indexNameCombo_, &QComboBox::currentTextChanged, this, [this](const QString&) {
        recomputeOreKey();
        recomputeDefaultSourceName();
    });
    connect(sourceNameEdit_, &QLineEdit::textEdited, this, [this](const QString&) {
        userEditedSource_ = true;
    });

    tabWidget_->addTab(tab, tr("Instrument"));

    populateCurrencyCombo();
    populateIndexNameCombo();
    populatePaymentFrequencyCombo();
    recomputeOreKey();
    recomputeDefaultSourceName();
}

void IrCurveEditor::buildProcessTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(8);

    // ===== 1. Header row: engine combo + segmented Simple/Advanced toggle (the toggle's
    // construction is copied from FxSpotRateEditor::buildBehaviourTab(), no IR-specific reason
    // for the affordance to differ).
    auto* headerRow = new QHBoxLayout();
    headerRow->addWidget(new QLabel(tr("Engine:"), tab));
    engineCombo_ = new QComboBox(tab);
    engineCombo_->setInsertPolicy(QComboBox::NoInsert);
    for (const auto& e : kEngines)
        engineCombo_->addItem(tr(e.label), QString::fromLatin1(e.code));
    {
        const int idx = engineCombo_->findData(QString::fromStdString(ir_.process_type));
        engineCombo_->setCurrentIndex(idx >= 0 ? idx : 0);
    }
    // Tooltip describes only the currently-selected engine, not every engine at once -- refreshed
    // on every selection change instead of one static blurb covering the whole combo.
    auto updateEngineTooltip = [this](int) {
        static const std::map<QString, QString> descriptions = {
            {QStringLiteral("VASICEK"),
             tr("Vasicek: dr = κ(θ−r)dt + σ dW. Constant volatility, mean-reverting; the rate can "
                "go negative.")},
            {QStringLiteral("COX_INGERSOLL_ROSS"),
             tr("Cox-Ingersoll-Ross: dr = κ(θ−r)dt + σ√r dW. Volatility scales with √r, so the "
                "rate stays non-negative (unlike Vasicek).")},
            {QStringLiteral("HULL_WHITE"),
             tr("Hull-White: like Vasicek, but supports a piecewise-constant mean level over "
                "time (a single constant level θ here, same as Vasicek's).")},
            {QStringLiteral("TWO_FACTOR_GAUSSIAN"),
             tr("Two-Factor Gaussian (G2++): r = x + y + θ, a sum of two correlated mean-reverting "
                "Ornstein-Uhlenbeck factors. Richer curve shapes (humps, inversions) than any "
                "single-factor model, at the cost of two reversion speeds and volatilities plus "
                "their correlation.")},
            {QStringLiteral("BLACK_KARASINSKI"),
             tr("Black-Karasinski: d ln r = κ(θ−ln r)dt + σ dW. The logarithm of the short rate is "
                "a mean-reverting Gaussian process, so the rate stays positive for all time; bond "
                "prices have no closed form and are evaluated on a trinomial lattice of ln r.")},
        };
        auto it = descriptions.find(engineCombo_->currentData().toString());
        engineCombo_->setToolTip(it != descriptions.end() ? it->second : QString());
    };
    updateEngineTooltip(engineCombo_->currentIndex());
    connect(engineCombo_, &QComboBox::currentIndexChanged, this, updateEngineTooltip);
    headerRow->addWidget(engineCombo_);

    // Prominent segmented Simple/Advanced toggle (right) -- mirrors FxSpotRateEditor's: both
    // pages edit the same definitions-driven parameter set, with the Advanced page's spins as
    // the single source of truth (currentParameters() and the save path read them).
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
    advancedBtn->setChecked(true); // Advanced is the default: the exact-entry surface, and the
                                   // mode the test scenario's spin-box steps exercise
    modeGroup_ = new QButtonGroup(this);
    modeGroup_->setExclusive(true);
    modeGroup_->addButton(simpleBtn, 0);
    modeGroup_->addButton(advancedBtn, 1);
    connect(modeGroup_, &QButtonGroup::idClicked, this, [this](int) { onModeChanged(); });

    auto* segRow = new QHBoxLayout();
    segRow->setSpacing(0); // connected segments
    segRow->addWidget(simpleBtn);
    segRow->addWidget(advancedBtn);
    headerRow->addStretch(1);
    headerRow->addLayout(segRow);
    layout->addLayout(headerRow);

    // ===== 2. Parameter table (Advanced mode): one row per
    // yield_curve_process_parameter_definition for the selected engine (4 for the one-factor
    // engines, 7 for Two-Factor Gaussian) -- read-only parameter cell (the definition's
    // description as tooltip) + QDoubleSpinBox per value, its range clamped to the definition's
    // min/max (NULL = unbounded) and seeded from the config's value row (edit mode) or the
    // definition's default_value. Rows are rebuilt by populateParameterRows() whenever the
    // engine changes; values are plain, real annualised numbers -- the mapping layer threads dt
    // (1/365 for this feed's day-per-tick convention, see ir_curve_template_resolver's own doc)
    // through separately, so nothing here is pre-scaled. These spins are the single source of
    // truth for both modes: the Simple page's sliders write through to them, and
    // currentParameters()/the save path read only them.
    parameterTable_ = new QTableWidget(0, 2, tab);
    parameterTable_->setHorizontalHeaderLabels({tr("Parameter"), tr("Value")});
    parameterTable_->verticalHeader()->setVisible(false);
    parameterTable_->verticalHeader()->setDefaultSectionSize(38);
    parameterTable_->horizontalHeader()->setStretchLastSection(true);
    parametersLoadingLabel_ = new QLabel(tr("Loading parameters…"), tab);
    parametersLoadingLabel_->setStyleSheet("color: gray; font-style: italic;");

    // ===== 3. Middle row: Simple/Advanced mode stack (left, compact) | curve-shape chart
    // (right, dominant -- a full LIBOR-style Curve Template can carry 10-12 tenor points now,
    // e.g. legacy USD-LIBOR-3M's DEPO/FRA-strip/swap-ladder grid, and needs real horizontal
    // room for its tenor-axis labels not to overlap).
    auto* middleRow = new QHBoxLayout();
    middleRow->setSpacing(12);

    modeStack_ = new QStackedWidget(tab);
    modeStack_->addWidget(buildSimpleParameterPage()); // index 0
    auto* advancedPage = new QWidget(tab);
    auto* advancedPageLayout = new QVBoxLayout(advancedPage);
    advancedPageLayout->setContentsMargins(0, 0, 0, 0);
    advancedPageLayout->addWidget(parameterTable_, 1);
    advancedPageLayout->addWidget(parametersLoadingLabel_);
    modeStack_->addWidget(advancedPage); // index 1
    modeStack_->setCurrentIndex(1);      // Advanced is the default mode (see the toggle above)
    modeStack_->setMaximumWidth(400);
    middleRow->addWidget(modeStack_, 0);

    auto* shapeBox = new QGroupBox(tr("Curve shape"), tab);
    shapeBox->setMinimumWidth(480);
    auto* shapeBoxLayout = new QVBoxLayout(shapeBox);
    shapeChart_ = new CurveShapePreviewChart(clientManager_, shapeBox);
    shapeChart_->setMinimumHeight(240);
    shapeBoxLayout->addWidget(shapeChart_);
    middleRow->addWidget(shapeBox, 1, Qt::AlignTop);
    layout->addLayout(middleRow);

    // ===== 4. Bottom row (full width): prominent sample-paths preview.
    auto* pathsBox = new QGroupBox(tr("Sample short-rate paths"), tab);
    auto* pathsBoxLayout = new QVBoxLayout(pathsBox);
    pathsChart_ = new SampleShortRatePathsChart(clientManager_, pathsBox);
    pathsChart_->setMinimumHeight(340);
    pathsChart_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    pathsBoxLayout->addWidget(pathsChart_);
    layout->addWidget(pathsBox, 1);

    connect(
        engineCombo_, &QComboBox::currentIndexChanged, this, [this]() { populateParameterRows(); });

    tabWidget_->addTab(tab, tr("Process"));

    populateParameterRows();
}

void IrCurveEditor::buildCurveTemplateTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);

    templateTable_ = new QTableWidget(0, 3, tab);
    templateTable_->setHorizontalHeaderLabels(
        {tr("Start Tenor"), tr("End Tenor"), tr("Instrument")});
    templateTable_->horizontalHeader()->setStretchLastSection(true);
    templateTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    templateTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    templateTable_->verticalHeader()->setVisible(false);
    templateTable_->verticalHeader()->setDefaultSectionSize(38); // matches FX's component table
    layout->addWidget(templateTable_, 1);

    auto* btnRow = new QHBoxLayout();
    addRowBtn_ = new QPushButton(tr("+ Add tenor entry"), tab);
    removeRowBtn_ = new QPushButton(tr("Remove"), tab);
    moveUpBtn_ = new QPushButton(tr("↑"), tab);
    moveDownBtn_ = new QPushButton(tr("↓"), tab);
    btnRow->addWidget(addRowBtn_);
    btnRow->addWidget(removeRowBtn_);
    btnRow->addWidget(moveUpBtn_);
    btnRow->addWidget(moveDownBtn_);
    btnRow->addStretch(1);
    layout->addLayout(btnRow);

    templateWarningLabel_ = new QLabel(tab);
    templateWarningLabel_->setStyleSheet("color:#d0a020;");
    templateWarningLabel_->setVisible(false);
    layout->addWidget(templateWarningLabel_);

    connect(addRowBtn_, &QPushButton::clicked, this, &IrCurveEditor::onAddTemplateRow);
    connect(removeRowBtn_, &QPushButton::clicked, this, &IrCurveEditor::onRemoveTemplateRow);
    connect(moveUpBtn_, &QPushButton::clicked, this, &IrCurveEditor::onMoveTemplateRowUp);
    connect(moveDownBtn_, &QPushButton::clicked, this, &IrCurveEditor::onMoveTemplateRowDown);

    tabWidget_->addTab(tab, tr("Curve Template"));

    populateTenorCodes();
    populateInstrumentCodes();
}

void IrCurveEditor::populateCurrencyCombo() {
    // setup_currency_combo() fetches codes asynchronously and applies flag icons -- same helper
    // FX/refdata detail dialogs use (e.g. BookDetailDialog's functional currency), rather than a
    // bespoke fetch+flag routine here.
    setup_currency_combo(currencyCombo_, this, clientManager_, imageCache_, [this]() {
        BOOST_LOG_SEV(lg(), debug)
            << "populateCurrencyCombo fallback_selection: ir_.currency_code='" << ir_.currency_code
            << "'";
        return QString::fromStdString(ir_.currency_code);
    });

    // Belt-and-braces re-assertion: live-tested and confirmed setup_currency_combo()'s own
    // setCurrentText(fallback) does not reliably stick on this editable+completer combo (observed
    // showing an unrelated alphabetically-first currency instead of the loaded entity's own).
    // Root cause found: for an editable QComboBox, setCurrentText() only sets the line edit's
    // raw text -- it does *not* search the model or update currentIndex. The flag icon shown is
    // tied to currentIndex, not to the displayed text, so setCurrentText() alone leaves the old
    // (index-0) flag showing even once the text itself reads correctly. setCurrentIndex(findText())
    // updates both. Forced again on a short delay once both async fetches (currency and index
    // name, which start around the same time) have had time to land.
    if (!ir_.currency_code.empty()) {
        QPointer<IrCurveEditor> self = this;
        QTimer::singleShot(500, this, [self]() {
            if (!self)
                return;
            const auto want = QString::fromStdString(self->ir_.currency_code);
            if (self->currencyCombo_->currentText() != want) {
                const int idx = self->currencyCombo_->findText(want);
                if (idx >= 0)
                    self->currencyCombo_->setCurrentIndex(idx);
                else
                    self->currencyCombo_->setCurrentText(want);
            }
        });
    }
}

void IrCurveEditor::populateIndexNameCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;
    auto task = [cm]() -> std::vector<std::string> {
        std::vector<std::string> codes;
        ores::refdata::messaging::get_floating_index_types_request req;
        req.limit = 1000;
        auto resp = cm->process_authenticated_request(req);
        if (resp && resp->success)
            for (const auto& fi : resp->types)
                codes.push_back(fi.code);
        return codes;
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        std::sort(codes.begin(), codes.end());
        self->knownFloatingIndexCodes_ = codes;

        // Prefer the combo's current selection (covers a user-driven currency change, which
        // fires normally); fall back to the loaded entity's own currency_code for the initial
        // population -- setup_currency_combo() populates the currency combo under a
        // QSignalBlocker (to avoid falsely dirtying the form), so currentTextChanged never fires
        // for that first, edit-mode population and this combo must not wait on it.
        auto ccy = self->currencyCombo_->currentText().toStdString();
        const auto comboCcy = ccy;
        if (ccy.empty())
            ccy = self->ir_.currency_code;
        const auto prefix = ccy + "-";

        const QSignalBlocker blocker(self->indexNameCombo_);
        const auto preselect = self->indexNameCombo_->currentText();
        self->indexNameCombo_->clear();
        for (const auto& code : codes) {
            if (!code.starts_with(prefix))
                continue;
            // Both shapes are valid curve identities: overnight-style
            // "<CCY>-<INDEX>" (e.g. USD-SOFR) and term-IBOR
            // "<CCY>-<INDEX>-<TENOR>" (e.g. USD-LIBOR-3M, legacy curves) --
            // the tenor segment here identifies *which* term-IBOR fixing
            // this curve tracks, distinct from the Curve Template's own
            // tenor points (which cover the curve's *shape*, not its
            // fixing identity).
            const auto suffix = code.substr(prefix.size());
            self->indexNameCombo_->addItem(QString::fromStdString(suffix));
        }
        BOOST_LOG_SEV(lg(), debug)
            << "populateIndexNameCombo: fetched=" << codes.size() << " comboCcy='" << comboCcy
            << "' entityCcy='" << self->ir_.currency_code << "' usedCcy='" << ccy
            << "' matched=" << self->indexNameCombo_->count() << " preselect='"
            << preselect.toStdString() << "'";

        // Preselect from the loaded entity on first population (edit mode), or restore whatever
        // the user had picked before a currency change repopulated this combo.
        QString wanted = preselect;
        if (wanted.isEmpty() && !self->ir_.index_family.empty() && self->ir_.currency_code == ccy) {
            wanted = joinIndexFamilyAndTenor(self->ir_.index_family, self->ir_.tenor);
        }
        if (!wanted.isEmpty())
            self->indexNameCombo_->setCurrentText(wanted);

        self->recomputeOreKey();
        self->recomputeDefaultSourceName();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void IrCurveEditor::populatePaymentFrequencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;
    auto task = [cm]() -> std::vector<std::string> {
        std::vector<std::string> codes;
        ores::refdata::messaging::get_payment_frequencies_request req;
        req.limit = 1000;
        auto resp = cm->process_authenticated_request(req);
        if (resp && resp->success)
            for (const auto& pf : resp->payment_frequencies)
                codes.push_back(pf.code);
        return codes;
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        std::sort(codes.begin(), codes.end());
        self->knownPaymentFrequencyCodes_ = codes;

        const QSignalBlocker blocker(self->fixedLegFrequencyCombo_);
        self->fixedLegFrequencyCombo_->clear();
        for (const auto& code : codes)
            self->fixedLegFrequencyCombo_->addItem(QString::fromStdString(code));
        const auto preselect = QString::fromStdString(self->ir_.fixed_leg_payment_frequency_code);
        if (!preselect.isEmpty())
            self->fixedLegFrequencyCombo_->setCurrentText(preselect);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void IrCurveEditor::populateTenorCodes() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;
    auto task = [cm]() -> std::vector<std::string> {
        std::vector<std::string> codes;
        ores::refdata::messaging::get_tenors_request req;
        req.limit = 1000;
        auto resp = cm->process_authenticated_request(req);
        if (resp && resp->success)
            for (const auto& t : resp->tenors)
                codes.push_back(t.code);
        return codes;
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        std::sort(codes.begin(), codes.end());
        self->knownTenorCodes_ = codes;
        self->syncTableFromModel();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void IrCurveEditor::populateInstrumentCodes() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;
    auto task = [cm]() -> std::vector<std::string> {
        std::vector<std::string> codes;
        ores::refdata::messaging::get_instrument_codes_request req;
        req.limit = 1000;
        auto resp = cm->process_authenticated_request(req);
        if (resp && resp->success)
            for (const auto& ic : resp->instruments)
                // Only DEPOSIT/FRA/SWAP-role codes price against a curve (see
                // instrument_code.curve_role's own doc) -- the great majority of the catalogue is
                // NONE and would just be noise in this combo.
                if (ic.curve_role != "NONE")
                    codes.push_back(ic.code);
        return codes;
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        std::sort(codes.begin(), codes.end());
        self->knownInstrumentCodes_ = codes;
        self->syncTableFromModel();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

QString IrCurveEditor::defaultSourceName() const {
    auto lower = [](std::string s) {
        std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
            return static_cast<char>(std::tolower(c));
        });
        return s;
    };
    const auto ccy = currencyCombo_->currentText().toStdString();
    const auto idx = indexNameCombo_->currentText().toStdString();
    if (ccy.empty() || idx.empty())
        return {};
    // Same shape as FxSpotRateEditor::defaultSourceName(): namespaced by collection only (to
    // avoid two collections' same currency+index colliding), not by asset class -- mirrors
    // ir_curve_generation_config.source_name's own doc comment on why.
    std::string collection = lower(feedName_.toStdString());
    collection.erase(std::remove(collection.begin(), collection.end(), ' '), collection.end());
    return QString::fromStdString("synthetic." + collection + "." + lower(ccy) + lower(idx));
}

void IrCurveEditor::recomputeOreKey() {
    const auto ccy = currencyCombo_->currentText().toStdString();
    const auto idx = indexNameCombo_->currentText().toStdString();
    if (ccy.empty() || idx.empty()) {
        oreKeyLabel_->setText({});
        return;
    }
    // indexNameCombo_ already stores the "<CCY>-" prefix stripped off (see
    // populateIndexNameCombo()), so ccy + "/" + idx is already the same qualifier
    // SyntheticBindingDialog::loadConfigs() and ir_curve_feed.cpp compute -- no stripping needed
    // here, unlike SyntheticBindingDialog's own copy which starts from the full stored
    // index_name. "RATES/YIELD/" matches ir_curve_feed.cpp's own find_series("RATES", "YIELD",
    // qualifier) call and FX's equivalent full SERIES_TYPE/METRIC/QUALIFIER shape (e.g.
    // "FX/RATE/EUR/USD").
    oreKeyLabel_->setText(QString::fromStdString("RATES/YIELD/" + ccy + "/" + idx));
}

void IrCurveEditor::recomputeDefaultSourceName() {
    if (userEditedSource_)
        return;
    const auto def = defaultSourceName();
    if (!def.isEmpty()) {
        const QSignalBlocker blocker(sourceNameEdit_);
        sourceNameEdit_->setText(def);
    }
}

void IrCurveEditor::onProcessFieldChanged() {
    refreshCharts();
}

QDoubleSpinBox* IrCurveEditor::valueSpinAt(int row) const {
    return qobject_cast<QDoubleSpinBox*>(parameterTable_->cellWidget(row, 1));
}

void IrCurveEditor::updatePriceSourceEnablement() {
    const bool vintage = priceSourceGroup_ && priceSourceGroup_->checkedId() == 1;
    BOOST_LOG_SEV(lg(), debug) << "updatePriceSourceEnablement: checkedId="
                               << (priceSourceGroup_ ? priceSourceGroup_->checkedId() : -1)
                               << ", vintage=" << vintage;
    if (vintageSourceEdit_)
        vintageSourceEdit_->setEnabled(vintage);
    if (vintageDateEdit_)
        vintageDateEdit_->setEnabled(vintage);
    if (browseVintageButton_)
        browseVintageButton_->setEnabled(vintage);
    // The server resolves the initial_rate parameter from a real observation when price_source
    // is "vintage" (see ir_curve_generation_config.price_source), so its row's spin box is
    // read-only then; every other parameter stays editable. rebuildParameterTable() re-runs this
    // after each population, so the spin may not exist yet on the earliest calls. The Simple
    // page's initial_rate row follows the same rule.
    if (initialRateSpin_)
        initialRateSpin_->setEnabled(!vintage);
    // buildInstrumentTab() runs this before the Process tab exists, so the simple table may not
    // be built yet on the earliest calls -- same guard as the widgets above.
    if (simpleParameterTable_) {
        for (int row = 0; row < simpleParameterTable_->rowCount(); ++row) {
            if (auto* nameItem = simpleParameterTable_->item(row, 0);
                nameItem && nameItem->text() == QStringLiteral("initial_rate")) {
                if (auto* slider = simpleSliderAt(row))
                    slider->setEnabled(!vintage);
                if (auto* spin = simpleSpinAt(row))
                    spin->setEnabled(!vintage);
            }
        }
    }
}

void IrCurveEditor::populateParameterRows() {
    const auto engine = engineCombo_->currentData().toString().toStdString();
    BOOST_LOG_SEV(lg(), debug) << "populateParameterRows: engine='" << engine << "'";

    if (!clientManager_ || !clientManager_->isConnected()) {
        parametersLoadingLabel_->setText(tr("Parameters unavailable — not connected."));
        return;
    }

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;

    struct FetchResult {
        bool success = false;
        std::vector<synthetic::domain::yield_curve_process_parameter_definition> definitions;
        std::vector<synthetic::domain::ir_curve_generation_config_process_parameter_value> values;
        QString error;
    };

    auto task =
        [cm, engine, configId = boost::uuids::to_string(ir_.id), isNew = isNew_]() -> FetchResult {
        namespace m = synthetic::messaging;
        FetchResult r;

        auto defsResp = cm->process_authenticated_request(
            m::get_yield_curve_process_parameter_definitions_request{.offset = 0, .limit = 1000});
        if (!defsResp) {
            r.error = QString::fromStdString(defsResp.error());
            return r;
        }
        for (auto& d : defsResp->parameter_definitions) {
            auto code = d.process_type_code;
            std::transform(code.begin(), code.end(), code.begin(), [](unsigned char c) {
                return static_cast<char>(std::toupper(c));
            });
            if (code == engine)
                r.definitions.push_back(std::move(d));
        }
        std::sort(r.definitions.begin(), r.definitions.end(), [](const auto& a, const auto& b) {
            return a.display_order < b.display_order;
        });

        // Only an existing config has stored value rows; a new one starts from the definitions'
        // default_value.
        if (!isNew) {
            auto valuesResp = cm->process_authenticated_request(
                m::get_ir_curve_generation_config_process_parameter_values_request{.offset = 0,
                                                                                   .limit = 1000});
            if (!valuesResp) {
                r.error = QString::fromStdString(valuesResp.error());
                return r;
            }
            for (auto& v : valuesResp->process_parameter_values)
                if (boost::uuids::to_string(v.config_id) == configId)
                    r.values.push_back(std::move(v));
        }
        r.success = true;
        return r;
    };

    parametersLoadingLabel_->setVisible(true);
    parametersLoadingLabel_->setText(tr("Loading parameters…"));
    parameterTable_->setEnabled(false);
    simpleParameterTable_->setEnabled(false);

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished, self, [self, watcher, engine]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        self->parameterTable_->setEnabled(true);
        self->simpleParameterTable_->setEnabled(true);
        if (!result.success) {
            self->parametersLoadingLabel_->setText(
                self->tr("Failed to load parameters: %1").arg(result.error));
            return;
        }
        self->parameterDefinitions_ = std::move(result.definitions);
        self->valueRows_ = std::move(result.values);
        self->rebuildParameterTable();
        if (self->parameterDefinitions_.empty()) {
            self->parametersLoadingLabel_->setVisible(true);
            self->parametersLoadingLabel_->setText(
                self->tr("No parameter definitions for engine '%1'.")
                    .arg(QString::fromStdString(engine)));
        } else {
            self->parametersLoadingLabel_->setVisible(false);
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void IrCurveEditor::rebuildParameterTable() {
    std::map<boost::uuids::uuid, double> valuesByDefinition;
    for (const auto& v : valueRows_)
        valuesByDefinition[v.parameter_definition_id] = v.parameter_value;

    const QSignalBlocker blocker(parameterTable_);
    parameterTable_->setRowCount(0);
    initialRateSpin_ = nullptr;
    for (const auto& d : parameterDefinitions_) {
        const int row = parameterTable_->rowCount();
        parameterTable_->insertRow(row);

        auto* nameItem = new QTableWidgetItem(QString::fromStdString(d.parameter_name));
        nameItem->setFlags(nameItem->flags() & ~Qt::ItemIsEditable);
        nameItem->setToolTip(QString::fromStdString(d.description));
        parameterTable_->setItem(row, 0, nameItem);

        auto* spin = new QDoubleSpinBox(parameterTable_);
        spin->setDecimals(6);
        // Definitions carry min/max as optional bounds (NULL = unbounded); a generous ±1e6
        // fallback keeps the spin usable for genuinely unbounded parameters (e.g. a mean level).
        spin->setMinimum(d.min_value ? *d.min_value : -1e6);
        spin->setMaximum(d.max_value ? *d.max_value : 1e6);
        const auto vit = valuesByDefinition.find(d.id);
        spin->setValue(vit != valuesByDefinition.end() ? vit->second : d.default_value);
        spin->setToolTip(QString::fromStdString(d.description));
        parameterTable_->setCellWidget(row, 1, spin);
        connect(spin, qOverload<double>(&QDoubleSpinBox::valueChanged), this, [this, row](double v) {
            // Mirror into the Simple page's row (blocked -- the Advanced spin is the source of
            // truth), then drive the charts.
            if (auto* s = simpleSpinAt(row)) {
                const QSignalBlocker blocker(s);
                s->setValue(v);
            }
            if (auto* sl = simpleSliderAt(row)) {
                const QSignalBlocker blocker(sl);
                sl->setValue(doubleToSliderValue(v));
            }
            onProcessFieldChanged();
        });
        if (d.parameter_name == "initial_rate")
            initialRateSpin_ = spin;
    }
    rebuildSimpleParameterTable();
    updatePriceSourceEnablement();
    refreshCharts();
}

QWidget* IrCurveEditor::buildSimpleParameterPage() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);
    layout->setContentsMargins(0, 0, 0, 0);
    simpleParameterTable_ = new QTableWidget(0, 2, page);
    simpleParameterTable_->setHorizontalHeaderLabels({tr("Parameter"), tr("Value")});
    simpleParameterTable_->verticalHeader()->setVisible(false);
    simpleParameterTable_->verticalHeader()->setDefaultSectionSize(38);
    simpleParameterTable_->horizontalHeader()->setStretchLastSection(true);
    simpleParameterTable_->setMaximumWidth(400);
    layout->addWidget(simpleParameterTable_, 1);
    return page;
}

QSlider* IrCurveEditor::simpleSliderAt(int row) const {
    auto* cell = qobject_cast<QWidget*>(simpleParameterTable_->cellWidget(row, 1));
    return cell ? cell->findChild<QSlider*>() : nullptr;
}

QDoubleSpinBox* IrCurveEditor::simpleSpinAt(int row) const {
    auto* cell = qobject_cast<QWidget*>(simpleParameterTable_->cellWidget(row, 1));
    return cell ? cell->findChild<QDoubleSpinBox*>() : nullptr;
}

void IrCurveEditor::rebuildSimpleParameterTable() {
    // Mirror the Advanced table row-for-row (same definitions, same order), but give each value
    // cell a slider + precise spin combo seeded from the Advanced spin's value. The slider range
    // comes from the definition's bounds where it has them, else a window around the current
    // value -- deliberately approximate, since a slider cannot express 6 decimals: the spin next
    // to it is the exact-entry surface, and the Advanced page stays the source of truth for
    // values an unbounded slider cannot represent (its range clamps at int limits).
    simpleParameterTable_->setRowCount(0);
    for (int row = 0; row < static_cast<int>(parameterDefinitions_.size()); ++row) {
        const auto& d = parameterDefinitions_[row];
        auto* advSpin = valueSpinAt(row);
        const double v = advSpin ? advSpin->value() : d.default_value;
        const double lo = d.min_value ? *d.min_value : v - 2.0 * std::max(1.0, std::abs(v));
        const double hi = d.max_value ? *d.max_value : v + 2.0 * std::max(1.0, std::abs(v));

        const int trow = simpleParameterTable_->rowCount();
        simpleParameterTable_->insertRow(trow);

        auto* nameItem = new QTableWidgetItem(QString::fromStdString(d.parameter_name));
        nameItem->setFlags(nameItem->flags() & ~Qt::ItemIsEditable);
        nameItem->setToolTip(QString::fromStdString(d.description));
        simpleParameterTable_->setItem(trow, 0, nameItem);

        auto* valueCell = new QWidget(simpleParameterTable_);
        auto* cellLayout = new QHBoxLayout(valueCell);
        cellLayout->setContentsMargins(4, 4, 4, 4);
        cellLayout->setSpacing(6);

        auto* slider = new QSlider(Qt::Horizontal, valueCell);
        slider->setRange(doubleToSliderValue(lo), doubleToSliderValue(hi));
        slider->setValue(doubleToSliderValue(v));
        slider->setMinimumWidth(110);
        slider->setToolTip(QString::fromStdString(d.description));
        connect(slider, &QSlider::valueChanged, this, [this, row](int sv) {
            syncSimpleRowToAdvanced(row, sliderValueToDouble(sv));
        });

        auto* spin = new QDoubleSpinBox(valueCell);
        spin->setDecimals(6);
        spin->setMinimum(lo);
        spin->setMaximum(hi);
        spin->setValue(v);
        spin->setToolTip(QString::fromStdString(d.description));
        connect(spin,
                qOverload<double>(&QDoubleSpinBox::valueChanged),
                this,
                [this, row](double v) { syncSimpleRowToAdvanced(row, v); });

        cellLayout->addWidget(slider, 1);
        cellLayout->addWidget(spin);
        simpleParameterTable_->setCellWidget(trow, 1, valueCell);
    }
}

void IrCurveEditor::syncSimpleRowToAdvanced(int row, double value) {
    // The Advanced page's spins are the single source of truth: mirror this row's new value into
    // the Advanced spin and the row's other Simple widget without re-entrancy, then let
    // onProcessFieldChanged() drive the charts.
    if (auto* advSpin = valueSpinAt(row)) {
        const QSignalBlocker blocker(advSpin);
        advSpin->setValue(value);
    }
    if (auto* spin = simpleSpinAt(row)) {
        const QSignalBlocker blocker(spin);
        spin->setValue(value);
    }
    if (auto* slider = simpleSliderAt(row)) {
        const QSignalBlocker blocker(slider);
        slider->setValue(doubleToSliderValue(value));
    }
    onProcessFieldChanged();
}

void IrCurveEditor::onModeChanged() {
    const int id = modeGroup_ ? modeGroup_->checkedId() : 1;
    // Leaving the Simple page costs nothing (its edits were already mirrored into the Advanced
    // spins); entering it rebuilds from the Advanced spins in case the parameter set changed
    // while hidden -- engine switches rebuild both pages, so this covers the initial population
    // and any future source of drift.
    if (id == 0)
        rebuildSimpleParameterTable();
    modeStack_->setCurrentIndex(id);
    refreshCharts();
}

std::vector<ores::synthetic::messaging::parameter_spec> IrCurveEditor::currentParameters() const {
    std::vector<ores::synthetic::messaging::parameter_spec> params;
    params.reserve(parameterDefinitions_.size());
    for (int row = 0;
         row < parameterTable_->rowCount() && row < static_cast<int>(parameterDefinitions_.size());
         ++row) {
        if (auto* spin = valueSpinAt(row))
            params.push_back({parameterDefinitions_[row].parameter_name, spin->value()});
    }
    return params;
}

void IrCurveEditor::onTemplateChanged() {
    rebuildModelFromTable();
    refreshCharts();
}

void IrCurveEditor::refreshCharts() {
    if (syncing_)
        return;
    // populateParameterRows() is still in flight (engine change) -- the fetch's landing
    // rebuilds the table and calls this again with the new engine's parameters, so skip the
    // stale ones rather than previewing the previous engine's rows under the new selection.
    if (parameterDefinitions_.empty())
        return;
    const auto engine = engineCombo_->currentData().toString().toStdString();
    const auto params = currentParameters();

    pathsChart_->setParameters(engine, params);
    pathsChart_->scheduleRefresh();

    std::vector<CurveShapePreviewChart::TemplateRow> rows;
    int seq = 0;
    for (const auto& e : entries_) {
        rows.push_back(CurveShapePreviewChart::TemplateRow{
            seq++, e.start_tenor_code, e.end_tenor_code, e.instrument_code});
    }
    shapeChart_->setParameters(
        engine, params, fixedLegFrequencyCombo_->currentText().toStdString(), rows);
    shapeChart_->scheduleRefresh();
}

namespace {
// Frameless, transparent-background combo -- same styling FxSpotRateEditor's component table
// applies to its own cell combos, so table-embedded combos look consistent across editors.
void styleTableCombo(QComboBox* combo) {
    combo->setStyleSheet(QStringLiteral("QComboBox { border: none; background: transparent; }"));
}
}

void IrCurveEditor::syncTableFromModel() {
    syncing_ = true;
    templateTable_->setRowCount(0);
    for (const auto& e : entries_) {
        const int row = templateTable_->rowCount();
        templateTable_->insertRow(row);

        auto* startCombo = new QComboBox(templateTable_);
        startCombo->addItem(QStringLiteral("SPOT"));
        for (const auto& code : knownTenorCodes_)
            if (code != "SPOT")
                startCombo->addItem(QString::fromStdString(code));
        startCombo->setCurrentText(QString::fromStdString(e.start_tenor_code));
        styleTableCombo(startCombo);
        templateTable_->setCellWidget(row, ColStart, startCombo);

        auto* endCombo = new QComboBox(templateTable_);
        for (const auto& code : knownTenorCodes_)
            endCombo->addItem(QString::fromStdString(code));
        endCombo->setCurrentText(QString::fromStdString(e.end_tenor_code));
        styleTableCombo(endCombo);
        templateTable_->setCellWidget(row, ColEnd, endCombo);

        auto* instrCombo = new QComboBox(templateTable_);
        for (const auto& code : knownInstrumentCodes_)
            instrCombo->addItem(QString::fromStdString(code));
        instrCombo->setCurrentText(QString::fromStdString(e.instrument_code));
        styleTableCombo(instrCombo);
        templateTable_->setCellWidget(row, ColInstrument, instrCombo);

        connect(
            startCombo, &QComboBox::currentTextChanged, this, &IrCurveEditor::onTemplateChanged);
        connect(endCombo, &QComboBox::currentTextChanged, this, &IrCurveEditor::onTemplateChanged);
        connect(
            instrCombo, &QComboBox::currentTextChanged, this, &IrCurveEditor::onTemplateChanged);
    }
    syncing_ = false;
    refreshCharts();
}

void IrCurveEditor::rebuildModelFromTable() {
    for (int row = 0; row < templateTable_->rowCount() && row < static_cast<int>(entries_.size());
         ++row) {
        auto* startCombo = qobject_cast<QComboBox*>(templateTable_->cellWidget(row, ColStart));
        auto* endCombo = qobject_cast<QComboBox*>(templateTable_->cellWidget(row, ColEnd));
        auto* instrCombo = qobject_cast<QComboBox*>(templateTable_->cellWidget(row, ColInstrument));
        if (startCombo)
            entries_[row].start_tenor_code = startCombo->currentText().toStdString();
        if (endCombo)
            entries_[row].end_tenor_code = endCombo->currentText().toStdString();
        if (instrCombo)
            entries_[row].instrument_code = instrCombo->currentText().toStdString();
    }
}

void IrCurveEditor::onAddTemplateRow() {
    rebuildModelFromTable();
    entries_.push_back(TemplateRow{{}, "SPOT", "1Y", "DEPO"});
    syncTableFromModel();
}

void IrCurveEditor::onRemoveTemplateRow() {
    rebuildModelFromTable();
    const auto row = templateTable_->currentRow();
    if (row < 0 || row >= static_cast<int>(entries_.size()))
        return;
    entries_.erase(entries_.begin() + row);
    syncTableFromModel();
}

void IrCurveEditor::onMoveTemplateRowUp() {
    rebuildModelFromTable();
    const auto row = templateTable_->currentRow();
    if (row <= 0 || row >= static_cast<int>(entries_.size()))
        return;
    std::swap(entries_[row - 1], entries_[row]);
    syncTableFromModel();
    templateTable_->selectRow(row - 1);
}

void IrCurveEditor::onMoveTemplateRowDown() {
    rebuildModelFromTable();
    const auto row = templateTable_->currentRow();
    if (row < 0 || row + 1 >= static_cast<int>(entries_.size()))
        return;
    std::swap(entries_[row], entries_[row + 1]);
    syncTableFromModel();
    templateTable_->selectRow(row + 1);
}

void IrCurveEditor::onBrowseVintageClicked() {
    BOOST_LOG_SEV(lg(), info) << "Browse available vintages clicked.";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Browse vintages aborted: not connected.";
        QMessageBox::warning(
            this, tr("Disconnected"), tr("Cannot browse vintages while disconnected."));
        return;
    }

    const auto ccy = currencyCombo_->currentText().toStdString();
    const auto idx = indexNameCombo_->currentText().toStdString();
    if (ccy.empty() || idx.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Browse vintages aborted: currency='" << ccy << "', index='"
                                  << idx << "' (one or both empty).";
        QMessageBox::warning(this, tr("Incomplete"), tr("Pick currency and index name first."));
        return;
    }
    // Mirrors ir_curve_feed.cpp's own strip_currency_prefix(): idx already excludes the
    // "<CCY>-" prefix (see indexNameCombo_'s own population), so no stripping needed here.
    const std::string qualifier = ccy + "/" + idx;
    BOOST_LOG_SEV(lg(), info) << "Fetching vintages for qualifier '" << qualifier << "'.";

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;

    // Unlike FX's single SPOT point per vintage, an IR curve has one observation per DEPOSIT
    // tenor -- point_id is shown (not filtered to a single value) so the tester can see which
    // tenor(s) a given (source, date) actually covers.
    struct Vintage {
        std::string source;
        std::string date;
        std::string point_id;
    };
    struct FetchResult {
        bool success = false;
        std::vector<Vintage> vintages;
        QString error;
    };

    auto task = [cm, qualifier]() -> FetchResult {
        namespace m = ores::marketdata::messaging;
        m::get_market_series_request series_req;
        series_req.limit = 10000;
        auto series_resp = cm->process_authenticated_request(series_req);
        if (!series_resp) {
            BOOST_LOG_SEV(lg(), error)
                << "get_market_series_request failed: " << series_resp.error();
            return {.success = false,
                    .vintages = {},
                    .error = QString::fromStdString(series_resp.error())};
        }
        BOOST_LOG_SEV(lg(), debug) << "get_market_series_request returned "
                                   << series_resp->market_series.size() << " series.";

        std::string series_id;
        for (const auto& s : series_resp->market_series) {
            if (s.series_type == "RATES" && s.metric == "YIELD" && s.qualifier == qualifier) {
                series_id = boost::uuids::to_string(s.id);
                break;
            }
        }
        if (series_id.empty()) {
            BOOST_LOG_SEV(lg(), info)
                << "No RATES/YIELD series found for qualifier '" << qualifier << "'.";
            return {.success = true, .vintages = {}, .error = {}};
        }
        BOOST_LOG_SEV(lg(), debug)
            << "Matched series_id=" << series_id << " for qualifier '" << qualifier << "'.";

        m::get_market_observations_by_series_id_request obs_req;
        obs_req.series_id = series_id;
        obs_req.limit = 10000;
        auto obs_resp = cm->process_authenticated_request(obs_req);
        if (!obs_resp) {
            BOOST_LOG_SEV(lg(), error)
                << "get_market_observations_by_series_id_request failed: " << obs_resp.error();
            return {.success = false,
                    .vintages = {},
                    .error = QString::fromStdString(obs_resp.error())};
        }
        BOOST_LOG_SEV(lg(), debug) << "get_market_observations_by_series_id_request returned "
                                   << obs_resp->market_observations.size() << " observations.";

        std::set<std::tuple<std::string, std::string, std::string>> seen;
        std::vector<Vintage> vintages;
        for (const auto& obs : obs_resp->market_observations) {
            const auto days = std::chrono::floor<std::chrono::days>(obs.observation_datetime);
            const auto date = std::format("{:%F}", days);
            if (seen.emplace(obs.source, date, obs.point_id).second)
                vintages.push_back({obs.source, date, obs.point_id});
        }
        std::sort(vintages.begin(), vintages.end(), [](const auto& a, const auto& b) {
            return std::tie(a.source, a.date) > std::tie(b.source, b.date); // newest first
        });
        BOOST_LOG_SEV(lg(), info) << "Resolved " << vintages.size()
                                  << " distinct vintage(s) for qualifier '" << qualifier << "'.";
        return {.success = true, .vintages = std::move(vintages), .error = {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished, self, [self, watcher, ccy, idx]() {
        auto result = watcher->result();
        watcher->deleteLater();
        BOOST_LOG_SEV(lg(), debug)
            << "Browse vintages fetch finished: success=" << result.success
            << ", count=" << result.vintages.size() << ", self-alive=" << (self != nullptr);
        if (!self)
            return;
        if (!result.success) {
            QMessageBox::warning(self, self->tr("Failed to fetch vintages"), result.error);
            return;
        }
        if (result.vintages.empty()) {
            QMessageBox::information(
                self,
                self->tr("No vintages found"),
                self->tr("No market data has been imported yet for RATES/YIELD/%1/%2. Run an "
                         "import first, then browse again.")
                    .arg(QString::fromStdString(ccy), QString::fromStdString(idx)));
            return;
        }

        QDialog dialog(self);
        dialog.setWindowTitle(self->tr("Available vintages for %1/%2")
                                  .arg(QString::fromStdString(ccy), QString::fromStdString(idx)));
        auto* layout = new QVBoxLayout(&dialog);
        auto* list = new QListWidget(&dialog);
        for (const auto& v : result.vintages) {
            list->addItem(QString("%1 — %2 (%3)")
                              .arg(QString::fromStdString(v.source),
                                   QString::fromStdString(v.date),
                                   QString::fromStdString(v.point_id)));
        }
        list->setCurrentRow(0);
        layout->addWidget(list);
        auto* box = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
        layout->addWidget(box);
        connect(box, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
        connect(box, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
        connect(list, &QListWidget::itemDoubleClicked, &dialog, &QDialog::accept);

        if (dialog.exec() == QDialog::Accepted && list->currentRow() >= 0) {
            const auto& chosen = result.vintages[static_cast<std::size_t>(list->currentRow())];
            self->vintageSourceEdit_->setText(QString::fromStdString(chosen.source));
            self->vintageDateEdit_->setText(QString::fromStdString(chosen.date));
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void IrCurveEditor::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        QMessageBox::warning(
            this, tr("Disconnected"), tr("Cannot save while disconnected from the server."));
        return;
    }

    rebuildModelFromTable();

    const auto ccy = currencyCombo_->currentText().toStdString();
    const auto idx = indexNameCombo_->currentText().toStdString();
    if (ccy.empty() || idx.empty()) {
        QMessageBox::warning(
            this, tr("Incomplete"), tr("Both currency and index name are required."));
        return;
    }
    if (entries_.empty()) {
        QMessageBox::warning(
            this, tr("Incomplete"), tr("At least one Curve Template entry is required."));
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

    auto ir = ir_;
    ir.currency_code = ccy;
    // indexNameCombo_ shows the floating_index_type suffix shape (e.g. "SOFR",
    // "LIBOR-3M") -- split it at the oresmd index_family/tenor boundary here.
    std::tie(ir.index_family, ir.tenor) = splitIndexFamilyAndTenor(idx);
    ir.role = roleCombo_->currentData().toString().toStdString();
    ir.process_type = engineCombo_->currentData().toString().toStdString();
    ir.fixed_leg_payment_frequency_code = fixedLegFrequencyCombo_->currentText().toStdString();
    if (vintageMode) {
        ir.price_source = "vintage";
        ir.vintage_source = vintageSourceEdit_->text().trimmed().toStdString();
        ir.vintage_date = vintageDateEdit_->text().trimmed().toStdString();
    } else {
        ir.price_source = "fixed";
        ir.vintage_source.clear();
        ir.vintage_date.clear();
    }
    ir.source_name = sourceNameEdit_->text().trimmed().toStdString();
    if (ir.source_name.empty())
        ir.source_name = defaultSourceName().toStdString();
    ir.ticks_per_hour =
        std::max(1, static_cast<int>(std::lround(3600.0 / std::max(1, secondsSpin_->value()))));
    ir.enabled = enabledCheck_->isChecked();
    ir.party_id = clientManager_->currentPartyId();
    ir.modified_by = username_.toStdString();
    ir.change_reason_code = crSel->reason_code;
    ir.change_commentary =
        crSel->commentary.empty() ? "Authored via Market Simulator" : crSel->commentary;
    ir.version = 0;

    std::vector<synthetic::domain::ir_curve_template_entry> entries;
    std::vector<std::string> keptIds;
    int seq = 0;
    for (const auto& row : entries_) {
        synthetic::domain::ir_curve_template_entry e;
        const bool rowIsNew = row.id.empty();
        e.id = boost::uuids::random_generator()();
        if (!rowIsNew) {
            try {
                e.id = boost::lexical_cast<boost::uuids::uuid>(row.id);
            } catch (...) {
                e.id = boost::uuids::random_generator()();
            }
        }
        e.ir_curve_config_id = ir.id;
        e.party_id = clientManager_->currentPartyId();
        e.sequence_index = seq++;
        e.start_tenor_code = row.start_tenor_code;
        e.end_tenor_code = row.end_tenor_code;
        e.instrument_code = row.instrument_code;
        e.modified_by = username_.toStdString();
        namespace reason = ores::dq::domain::change_reason_constants::codes;
        e.change_reason_code =
            rowIsNew ? std::string(reason::new_record) : std::string(reason::non_material_update);
        e.change_commentary = "Authored via Market Simulator";
        e.version = 0;
        entries.push_back(e);
        if (!row.id.empty())
            keptIds.push_back(row.id);
    }

    std::vector<std::string> toDelete;
    for (const auto& origId : originalEntryIds_) {
        if (std::find(keptIds.begin(), keptIds.end(), origId) == keptIds.end())
            toDelete.push_back(origId);
    }

    // Row-based process parameters: one value row per parameter-table row, keyed onto its
    // definition; existing rows keep their id (stale ones are deleted below, mirroring the
    // entries flow). Only parameters actually present in the definitions catalogue are saved.
    std::vector<synthetic::domain::ir_curve_generation_config_process_parameter_value> values;
    std::vector<std::string> keptValueIds;
    for (int row = 0;
         row < parameterTable_->rowCount() && row < static_cast<int>(parameterDefinitions_.size());
         ++row) {
        auto* spin = valueSpinAt(row);
        if (!spin)
            continue;
        const auto& def = parameterDefinitions_[row];
        synthetic::domain::ir_curve_generation_config_process_parameter_value v;
        bool rowIsNew = true;
        const auto existingIt =
            std::find_if(valueRows_.begin(), valueRows_.end(), [&def](const auto& x) {
                return x.parameter_definition_id == def.id;
            });
        if (existingIt != valueRows_.end()) {
            v.id = existingIt->id;
            keptValueIds.push_back(boost::uuids::to_string(existingIt->id));
            rowIsNew = false;
        } else {
            v.id = boost::uuids::random_generator()();
        }
        v.config_id = ir.id;
        v.parameter_definition_id = def.id;
        v.parameter_value = spin->value();
        v.modified_by = username_.toStdString();
        namespace reason = ores::dq::domain::change_reason_constants::codes;
        v.change_reason_code =
            rowIsNew ? std::string(reason::new_record) : std::string(reason::non_material_update);
        v.change_commentary = "Authored via Market Simulator";
        v.version = 0;
        values.push_back(std::move(v));
    }

    std::vector<std::string> staleValueIds;
    for (const auto& existing : valueRows_) {
        const auto id = boost::uuids::to_string(existing.id);
        if (std::find(keptValueIds.begin(), keptValueIds.end(), id) == keptValueIds.end())
            staleValueIds.push_back(id);
    }

    const std::string irId = boost::uuids::to_string(ir.id);
    BOOST_LOG_SEV(lg(), info) << "Saving IR curve " << irId << " (" << ccy << " " << idx
                              << ", new=" << isNew_ << ") with " << entries.size()
                              << " Curve Template entries, deleting " << toDelete.size() << ", and "
                              << values.size() << " parameter value rows, deleting "
                              << staleValueIds.size() << ".";

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;

    struct SaveResult {
        bool success;
        QString message;
    };

    auto task = [cm, ir, entries, toDelete, values, staleValueIds]() -> SaveResult {
        namespace m = synthetic::messaging;

        auto irResp =
            cm->process_authenticated_request(m::save_ir_curve_generation_config_request::from(ir));
        if (!irResp)
            return {false, QString::fromStdString(irResp.error())};
        if (!irResp->success)
            return {false, QString::fromStdString(irResp->message)};

        if (!toDelete.empty()) {
            auto dResp = cm->process_authenticated_request(
                m::delete_ir_curve_template_entry_request{.ids = toDelete});
            if (!dResp)
                return {false, QString::fromStdString(dResp.error())};
            if (!dResp->success)
                return {false, QString::fromStdString(dResp->message)};
        }

        for (const auto& e : entries) {
            auto eResp =
                cm->process_authenticated_request(m::save_ir_curve_template_entry_request::from(e));
            if (!eResp)
                return {false, QString::fromStdString(eResp.error())};
            if (!eResp->success)
                return {false, QString::fromStdString(eResp->message)};
        }

        if (!staleValueIds.empty()) {
            auto vdResp = cm->process_authenticated_request(
                m::delete_ir_curve_generation_config_process_parameter_value_request{
                    .ids = staleValueIds});
            if (!vdResp)
                return {false, QString::fromStdString(vdResp.error())};
            if (!vdResp->success)
                return {false, QString::fromStdString(vdResp->message)};
        }

        for (const auto& v : values) {
            auto vResp = cm->process_authenticated_request(
                m::save_ir_curve_generation_config_process_parameter_value_request::from(v));
            if (!vResp)
                return {false, QString::fromStdString(vResp.error())};
            if (!vResp->success)
                return {false, QString::fromStdString(vResp->message)};
        }

        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher, irId]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Save failed for IR curve " << irId << ": " << result.message.toStdString();
            emit self->errorOccurred(result.message);
            QMessageBox::critical(self, self->tr("Save failed"), result.message);
            return;
        }
        BOOST_LOG_SEV(lg(), info) << "Saved IR curve " << irId << ".";
        emit self->savedOk();
        emit self->statusChanged(self->tr("IR curve saved."));
        self->notifySaveSuccess(self->tr("IR curve saved."));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}
