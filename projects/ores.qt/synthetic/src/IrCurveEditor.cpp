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
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include <QButtonGroup>
#include <QComboBox>
#include <QCompleter>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QRadioButton>
#include <QSizePolicy>
#include <QSlider>
#include <QSplitter>
#include <QStackedWidget>
#include <QTableWidget>
#include <QTimer>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr int ColStart = 0;
constexpr int ColEnd = 1;
constexpr int ColInstrument = 2;

const char* kEngines[] = {"VASICEK", "CIR", "HULL_WHITE"};

} // namespace

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
    // Day-scaled defaults (1 tick == 1 calendar day) matching the seeded Barclays curves'
    // magnitude -- see the Process tab's own slider-range comment for why kappa/sigma default to
    // ~0.05/~0.01 (annual-scale values, 30-1000x too large) would previously produce numerically
    // unstable discount factors at longer tenors.
    ir_.kappa = 0.001;
    ir_.theta = 0.03;
    ir_.sigma = 0.0005;
    ir_.initial_rate = 0.03;
    ir_.ticks_per_hour = 3600;
    ir_.enabled = true;
    ir_.fixed_leg_payment_frequency_code = "Annual";

    BOOST_LOG_SEV(lg(), info) << "Opening IR curve editor for a new curve "
                              << boost::uuids::to_string(ir_.id) << " under feed "
                              << boost::uuids::to_string(parentFeedId) << ".";

    // Seed a sensible default 3-entry template (short deposit, FRA, swap).
    entries_.push_back(TemplateRow{{}, "SPOT", "3M", "Deposit"});
    entries_.push_back(TemplateRow{{}, "3M", "6M", "ForwardRateAgreement"});
    entries_.push_back(TemplateRow{{}, "SPOT", "2Y", "Swap"});

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

    fixedLegFrequencyCombo_ = new QComboBox(identityBox);
    fixedLegFrequencyCombo_->setInsertPolicy(QComboBox::NoInsert);
    form->addRow(tr("Fixed leg frequency"), fixedLegFrequencyCombo_);

    enabledCheck_ = new QCheckBox(tr("Enabled"), identityBox);
    enabledCheck_->setChecked(ir_.enabled);
    form->addRow(QString(), enabledCheck_);

    secondsSpin_ = new QSpinBox(identityBox);
    secondsSpin_->setRange(1, 3600);
    secondsSpin_->setSuffix(tr(" s"));
    secondsSpin_->setValue(ir_.ticks_per_hour > 0 ?
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
    outer->addStretch(1);

    connect(currencyCombo_, &QComboBox::currentTextChanged, this, [this](const QString&) {
        populateIndexNameCombo();
        recomputeDefaultSourceName();
    });
    connect(indexNameCombo_, &QComboBox::currentTextChanged, this, [this](const QString&) {
        recomputeDefaultSourceName();
    });
    connect(sourceNameEdit_, &QLineEdit::textEdited, this, [this](const QString&) {
        userEditedSource_ = true;
    });

    tabWidget_->addTab(tab, tr("Instrument"));

    populateCurrencyCombo();
    populateIndexNameCombo();
    populatePaymentFrequencyCombo();
    recomputeDefaultSourceName();
}

void IrCurveEditor::buildProcessTab() {
    auto* tab = new QWidget(this);
    auto* layout = new QVBoxLayout(tab);
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(8);

    // ===== 1. Header row: engine + segmented Simple/Advanced toggle -- copied verbatim from
    // FxSpotRateEditor::buildBehaviourTab(), no IR-specific reason for this affordance to differ.
    auto* headerRow = new QHBoxLayout();
    headerRow->addWidget(new QLabel(tr("Engine:"), tab));
    engineCombo_ = new QComboBox(tab);
    engineCombo_->setInsertPolicy(QComboBox::NoInsert);
    for (const auto* e : kEngines)
        engineCombo_->addItem(QString::fromUtf8(e));
    engineCombo_->setCurrentText(QString::fromStdString(ir_.process_type));
    // Tooltip describes only the currently-selected engine, not all three at once -- refreshed on
    // every selection change instead of one static blurb covering the whole combo.
    auto updateEngineTooltip = [this](const QString& engine) {
        static const std::map<QString, QString> descriptions = {
            {QStringLiteral("VASICEK"),
             tr("Vasicek: dr = κ(θ−r)dt + σ dW. Constant volatility, mean-reverting; the rate can "
                "go negative.")},
            {QStringLiteral("CIR"),
             tr("Cox-Ingersoll-Ross: dr = κ(θ−r)dt + σ√r dW. Volatility scales with √r, so the "
                "rate stays non-negative (unlike Vasicek).")},
            {QStringLiteral("HULL_WHITE"),
             tr("Hull-White: like Vasicek, but supports a piecewise-constant mean level over "
                "time (a single constant level θ here, same as Vasicek's).")},
        };
        auto it = descriptions.find(engine);
        engineCombo_->setToolTip(it != descriptions.end() ? it->second : QString());
    };
    updateEngineTooltip(engineCombo_->currentText());
    connect(engineCombo_, &QComboBox::currentTextChanged, this, updateEngineTooltip);
    headerRow->addWidget(engineCombo_);
    headerRow->addStretch(1);

    auto* simpleBtn = new QPushButton(tr("Simple"), tab);
    simpleBtn->setToolTip(tr("Slider-driven, quick exploration -- values snap to the slider's "
                             "granularity, not for precise entry."));
    auto* advancedBtn = new QPushButton(tr("Advanced"), tab);
    advancedBtn->setToolTip(tr("Directly-editable table for typing exact parameter values."));
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
    modeGroup_ = new QButtonGroup(tab);
    modeGroup_->setExclusive(true);
    modeGroup_->addButton(simpleBtn, 0);
    modeGroup_->addButton(advancedBtn, 1);
    auto* segRow = new QHBoxLayout();
    segRow->setSpacing(0);
    segRow->addWidget(simpleBtn);
    segRow->addWidget(advancedBtn);
    headerRow->addLayout(segRow);
    layout->addLayout(headerRow);

    // ===== 2. Mode stack: Simple (slider-only, value echoed in a label -- precise entry is
    // Advanced-only, same philosophy as FX) vs Advanced (directly-editable table). Ranges sized
    // to this system's actual day-scaled convention (1 tick == 1 calendar day, see
    // ir_curve_template_resolver's own doc) -- the seeded Barclays curves run kappa
    // ~0.0007-0.0015, sigma ~0.0004-0.0007 (e.g. USD-SOFR: kappa=0.00151, sigma=0.00042). A wider
    // range would put every real value imperceptibly near zero, so a slider nudge would jump by
    // orders of magnitude into the numerically-unstable regime documented in the
    // seed-ir-curve-sample-data follow-on task.
    modeStack_ = new QStackedWidget(tab);

    auto* simplePage = new QWidget(modeStack_);
    auto* simpleLayout = new QVBoxLayout(simplePage);
    auto addParamRow = [&](const QString& labelText,
                           const QString& tip,
                           QSlider*& slider,
                           QDoubleSpinBox*& spin, // hidden value model, not shown -- see below
                           double minV,
                           double maxV,
                           double initial,
                           double step) {
        auto* header = new QHBoxLayout();
        auto* titleLabel = new QLabel(labelText, simplePage);
        titleLabel->setToolTip(tip);
        header->addWidget(titleLabel);
        header->addStretch(1);
        auto* valueLabel = new QLabel(simplePage);
        valueLabel->setStyleSheet("color: gray;");
        header->addWidget(valueLabel);
        simpleLayout->addLayout(header);

        slider = new QSlider(Qt::Horizontal, simplePage);
        slider->setRange(0, 1000);
        slider->setToolTip(tip);
        simpleLayout->addWidget(slider);

        // Spin box is the value model (read by save/charts/Advanced-table sync) but is never
        // added to a layout -- Simple mode is deliberately imprecise, matching FX's own Simple
        // page (a value label, not a spinbox, next to the slider).
        spin = new QDoubleSpinBox(simplePage);
        spin->setRange(minV, maxV);
        spin->setDecimals(6);
        spin->setSingleStep(step);
        spin->setValue(initial);
        spin->setVisible(false);
        slider->setValue(static_cast<int>(std::lround((initial - minV) / (maxV - minV) * 1000)));
        valueLabel->setText(QString::number(initial, 'g', 6));

        connect(slider, &QSlider::valueChanged, this, [spin, minV, maxV](int v) {
            const QSignalBlocker blocker(spin);
            spin->setValue(minV + (maxV - minV) * v / 1000.0);
        });
        connect(spin,
                qOverload<double>(&QDoubleSpinBox::valueChanged),
                this,
                [slider, valueLabel, minV, maxV](double v) {
                    const QSignalBlocker blocker(slider);
                    slider->setValue(
                        static_cast<int>(std::lround((v - minV) / (maxV - minV) * 1000)));
                    valueLabel->setText(QString::number(v, 'g', 6));
                });
        connect(spin,
                qOverload<double>(&QDoubleSpinBox::valueChanged),
                this,
                &IrCurveEditor::onProcessFieldChanged);
    };
    addParamRow(tr("Initial rate r0"),
               tr("The starting short rate the process simulates from -- where the curve begins."),
               initialRateSlider_,
               initialRateSpin_,
               -0.02,
               0.15,
               ir_.initial_rate,
               0.0001);
    addParamRow(tr("Mean level θ"),
               tr("The long-run level the short rate reverts toward. With κ=0 the rate never "
                  "reverts and just diffuses freely."),
               thetaSlider_,
               thetaSpin_,
               -0.02,
               0.15,
               ir_.theta,
               0.0001);
    addParamRow(tr("Reversion κ"),
               tr("Mean-reversion speed, per tick (1 tick = 1 day). Larger κ pulls the rate back "
                  "toward θ faster. Real seeded curves run κ ≈ 0.0007-0.0015 -- values much larger "
                  "than that make longer-tenor discount factors numerically unstable."),
               kappaSlider_,
               kappaSpin_,
               0.0,
               0.02,
               ir_.kappa,
               0.00001);
    addParamRow(tr("Volatility σ"),
               tr("Per-tick volatility of the short rate. Real seeded curves run σ ≈ "
                  "0.0004-0.0007."),
               sigmaSlider_,
               sigmaSpin_,
               0.0,
               0.005,
               ir_.sigma,
               0.00001);
    simpleLayout->addStretch(1);
    modeStack_->addWidget(simplePage);

    // Advanced page: same four values, one row of directly-editable, unclamped-precision cells --
    // for typing exact numbers a slider's granularity can't reach. The only precise-entry
    // surface, matching FX's Advanced component table's role.
    auto* advancedPage = new QWidget(modeStack_);
    auto* advancedLayout = new QVBoxLayout(advancedPage);
    advancedTable_ = new QTableWidget(1, 4, advancedPage);
    advancedTable_->setHorizontalHeaderLabels(
        {tr("r0"), tr("θ"), tr("κ"), tr("σ")});
    advancedTable_->verticalHeader()->setVisible(false);
    advancedTable_->verticalHeader()->setDefaultSectionSize(38);
    advancedTable_->horizontalHeader()->setStretchLastSection(true);
    advancedLayout->addWidget(advancedTable_);
    modeStack_->addWidget(advancedPage);

    connect(advancedTable_, &QTableWidget::itemChanged, this, [this](QTableWidgetItem* item) {
        bool ok = false;
        const double v = item->text().toDouble(&ok);
        if (!ok)
            return;
        QDoubleSpinBox* target = nullptr;
        switch (item->column()) {
            case 0: target = initialRateSpin_; break;
            case 1: target = thetaSpin_; break;
            case 2: target = kappaSpin_; break;
            case 3: target = sigmaSpin_; break;
        }
        if (target)
            target->setValue(v); // propagates to slider + label + charts via its own valueChanged
    });

    connect(modeGroup_, &QButtonGroup::idClicked, this, &IrCurveEditor::onModeChanged);

    // ===== 3. Middle row: mode stack (left, dominant) | compact curve-shape chart (right) --
    // same zone assignment as FX (compact chart is the supporting view, prominent bottom chart
    // is the hero view -- sample paths, matching FX's own "sample paths is the hero" choice).
    auto* middleRow = new QHBoxLayout();
    middleRow->setSpacing(12);
    middleRow->addWidget(modeStack_, 1);

    auto* shapeBox = new QGroupBox(tr("Curve shape"), tab);
    shapeBox->setMinimumWidth(380);
    shapeBox->setMaximumWidth(560); // tenor category labels need more room than a plain index did
    auto* shapeBoxLayout = new QVBoxLayout(shapeBox);
    shapeChart_ = new CurveShapePreviewChart(clientManager_, shapeBox);
    shapeChart_->setMinimumHeight(240);
    shapeBoxLayout->addWidget(shapeChart_);
    middleRow->addWidget(shapeBox, 0, Qt::AlignTop);
    layout->addLayout(middleRow);

    // ===== 4. Bottom row (full width): prominent sample-paths preview.
    auto* pathsBox = new QGroupBox(tr("Sample short-rate paths"), tab);
    auto* pathsBoxLayout = new QVBoxLayout(pathsBox);
    pathsChart_ = new SampleShortRatePathsChart(clientManager_, pathsBox);
    pathsChart_->setMinimumHeight(340);
    pathsChart_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    pathsBoxLayout->addWidget(pathsChart_);
    layout->addWidget(pathsBox, 1);

    connect(engineCombo_,
            &QComboBox::currentTextChanged,
            this,
            &IrCurveEditor::onProcessFieldChanged);

    tabWidget_->addTab(tab, tr("Process"));
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
        BOOST_LOG_SEV(lg(), debug) << "populateCurrencyCombo fallback_selection: ir_.currency_code='"
                                   << ir_.currency_code << "'";
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
            // Overnight-style indices are exactly "<CCY>-<INDEX>" (two segments); exclude
            // term-IBOR tenor variants ("<CCY>-<INDEX>-<TENOR>", three segments) -- those aren't a
            // single curve's identity, the curve's own Curve Template already carries tenors.
            const auto suffix = code.substr(prefix.size());
            if (suffix.find('-') != std::string::npos)
                continue;
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
        if (wanted.isEmpty() && !self->ir_.index_name.empty() && self->ir_.currency_code == ccy) {
            const auto ir_prefix = self->ir_.currency_code + "-";
            wanted = QString::fromStdString(self->ir_.index_name.starts_with(ir_prefix) ?
                                                self->ir_.index_name.substr(ir_prefix.size()) :
                                                self->ir_.index_name);
        }
        if (!wanted.isEmpty())
            self->indexNameCombo_->setCurrentText(wanted);

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
        const auto preselect =
            QString::fromStdString(self->ir_.fixed_leg_payment_frequency_code);
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
    // Keep the Advanced table's display in sync whenever a spin value changes, regardless of
    // which page is currently visible (e.g. programmatic updates while on the Simple page).
    if (!syncing_ && advancedTable_) {
        const QSignalBlocker blocker(advancedTable_);
        advancedTable_->setItem(
            0, 0, new QTableWidgetItem(QString::number(initialRateSpin_->value(), 'g', 8)));
        advancedTable_->setItem(
            0, 1, new QTableWidgetItem(QString::number(thetaSpin_->value(), 'g', 8)));
        advancedTable_->setItem(
            0, 2, new QTableWidgetItem(QString::number(kappaSpin_->value(), 'g', 8)));
        advancedTable_->setItem(
            0, 3, new QTableWidgetItem(QString::number(sigmaSpin_->value(), 'g', 8)));
    }
}

void IrCurveEditor::onModeChanged() {
    modeStack_->setCurrentIndex(modeGroup_->checkedId());
    if (modeGroup_->checkedId() == 1)
        onProcessFieldChanged(); // populate the Advanced table from the current spin values
}

void IrCurveEditor::onTemplateChanged() {
    rebuildModelFromTable();
    refreshCharts();
}

void IrCurveEditor::refreshCharts() {
    if (syncing_)
        return;
    const auto engine = engineCombo_->currentText().toStdString();
    const auto kappa = kappaSpin_->value();
    const auto theta = thetaSpin_->value();
    const auto sigma = sigmaSpin_->value();
    const auto r0 = initialRateSpin_->value();

    pathsChart_->setParameters(engine, kappa, theta, sigma, r0);
    pathsChart_->scheduleRefresh();

    std::vector<CurveShapePreviewChart::TemplateRow> rows;
    int seq = 0;
    for (const auto& e : entries_) {
        rows.push_back(CurveShapePreviewChart::TemplateRow{
            seq++, e.start_tenor_code, e.end_tenor_code, e.instrument_code});
    }
    shapeChart_->setParameters(engine,
                               kappa,
                               theta,
                               sigma,
                               r0,
                               fixedLegFrequencyCombo_->currentText().toStdString(),
                               rows);
    shapeChart_->scheduleRefresh();
}

namespace {
// Frameless, transparent-background combo -- same styling FxSpotRateEditor's component table
// applies to its own cell combos, so table-embedded combos look consistent across editors.
void styleTableCombo(QComboBox* combo) {
    combo->setStyleSheet(
        QStringLiteral("QComboBox { border: none; background: transparent; }"));
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

        connect(startCombo,
                &QComboBox::currentTextChanged,
                this,
                &IrCurveEditor::onTemplateChanged);
        connect(
            endCombo, &QComboBox::currentTextChanged, this, &IrCurveEditor::onTemplateChanged);
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
        auto* instrCombo =
            qobject_cast<QComboBox*>(templateTable_->cellWidget(row, ColInstrument));
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
    entries_.push_back(TemplateRow{{}, "SPOT", "1Y", "Deposit"});
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

    const auto crOpType = isNew_ ? ChangeReasonDialog::OperationType::Create :
                                   ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, true, isNew_ ? "system" : "common");
    if (!crSel)
        return;

    auto ir = ir_;
    ir.currency_code = ccy;
    // Stored as the full floating_index_type code (see the field's own doc comment) --
    // indexNameCombo_ shows just the suffix, so re-prefix it here.
    ir.index_name = ccy + "-" + idx;
    ir.process_type = engineCombo_->currentText().toStdString();
    ir.kappa = kappaSpin_->value();
    ir.theta = thetaSpin_->value();
    ir.sigma = sigmaSpin_->value();
    ir.initial_rate = initialRateSpin_->value();
    ir.fixed_leg_payment_frequency_code = fixedLegFrequencyCombo_->currentText().toStdString();
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

    const std::string irId = boost::uuids::to_string(ir.id);
    BOOST_LOG_SEV(lg(), info) << "Saving IR curve " << irId << " (" << ccy << " " << idx
                              << ", new=" << isNew_ << ") with " << entries.size()
                              << " Curve Template entries, deleting " << toDelete.size() << ".";

    QPointer<IrCurveEditor> self = this;
    auto* cm = clientManager_;

    struct SaveResult {
        bool success;
        QString message;
    };

    auto task = [cm, ir, entries, toDelete]() -> SaveResult {
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
