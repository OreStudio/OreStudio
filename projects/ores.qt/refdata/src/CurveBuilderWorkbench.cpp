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
#include "ores.qt/CurveBuilderWorkbench.hpp"
#include "ores.analytics.quant/service/curve_health_checker.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include "ores.analytics.quant/service/forward_rate_calculator.hpp"
#include "ores.marketdata.api/messaging/curve_republish_protocol.hpp"
#include "ores.qt/BootstrapConfigPickerDialog.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MarketSeriesPickerDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/floating_index_type_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_pillar_protocol.hpp"
#include <QComboBox>
#include <QDateTimeEdit>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPainter>
#include <QPointer>
#include <QPushButton>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>
#include <QtCharts/QBarCategoryAxis>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>

namespace ores::qt {

namespace {

QChart* make_chart(const QString& title) {
    auto* chart = new QChart();
    chart->setTheme(QChart::ChartThemeDark);
    chart->setBackgroundBrush(Qt::NoBrush);
    chart->setPlotAreaBackgroundVisible(false);
    chart->setTitle(title);
    chart->setMargins(QMargins(4, 4, 4, 4));
    return chart;
}

std::chrono::system_clock::time_point to_time_point(const QDateTime& dt) {
    return std::chrono::system_clock::from_time_t(static_cast<time_t>(dt.toSecsSinceEpoch()));
}

QString to_qstring(std::chrono::year_month_day date) {
    return QString("%1").arg(QString::fromStdString(std::format("{:%F}", date)));
}

}

CurveBuilderWorkbench::CurveBuilderWorkbench(QWidget* parent)
    : QWidget(parent) {
    buildUi();
    updateActionStates();
}

void CurveBuilderWorkbench::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadFloatingIndexTypes();
    loadTenors();
}

void CurveBuilderWorkbench::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    setup_flag_combo(this, currencyCombo_, imageCache_, FlagSource::Currency);
}

void CurveBuilderWorkbench::loadFloatingIndexTypes() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    refdata::messaging::get_floating_index_types_request request;
    request.offset = 0;
    request.limit = 1000;

    QPointer<CurveBuilderWorkbench> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager,
         request]() -> std::expected<refdata::messaging::get_floating_index_types_response, QString> {
            auto result = clientManager->process_authenticated_request(request);
            if (!result)
                return std::unexpected(QString::fromStdString(result.error()));
            if (!result->success)
                return std::unexpected(QString::fromStdString(result->message));
            return *result;
        });

    using ResultType =
        std::expected<refdata::messaging::get_floating_index_types_response, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result)
            return;
        self->floatingIndexTypes_ = std::move(result->types);
        self->refreshCurrencyCombo();
    });
    watcher->setFuture(future);
}

void CurveBuilderWorkbench::loadTenors() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CurveBuilderWorkbench> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager]() { return fetch_tenors(clientManager); });

    using ResultType = std::expected<std::vector<refdata::domain::tenor>, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result)
            return;
        // Sort by sort_order (explicit ladder position, e.g. O/N < S/N < SPOT < 1W < 1M < ... <
        // 30Y), not lexicographically -- alphabetical order scatters "12M" before "1Y" and puts
        // SPOT nowhere near the other short-dated special codes it belongs with. Also dedupe by
        // code, keeping the lowest sort_order seen: fetch_tenors() can return more than one row
        // per code (e.g. multiple tenant-scoped rows).
        auto tenors = std::move(*result);
        std::sort(tenors.begin(), tenors.end(), [](const auto& a, const auto& b) {
            return a.sort_order < b.sort_order;
        });
        self->tenorCodes_.clear();
        for (const auto& t : tenors) {
            if (std::find(self->tenorCodes_.begin(), self->tenorCodes_.end(), t.code) ==
                self->tenorCodes_.end())
                self->tenorCodes_.push_back(t.code);
        }
        self->addPillarButton_->setEnabled(true);
        self->addPillarButton_->setToolTip(QString());
    });
    watcher->setFuture(future);
}

QComboBox* CurveBuilderWorkbench::makeTenorCombo(const QString& initial) {
    auto* combo = new QComboBox(pillarsTable_);
    for (const auto& code : tenorCodes_)
        combo->addItem(QString::fromStdString(code));
    const int idx = combo->findText(initial);
    combo->setCurrentIndex(idx);
    return combo;
}

QComboBox* CurveBuilderWorkbench::makeCurveRoleCombo(const QString& initial) {
    auto* combo = new QComboBox(pillarsTable_);
    combo->addItems({"DEPOSIT", "FRA", "SWAP"});
    combo->setCurrentText(initial.isEmpty() ? "DEPOSIT" : initial);
    return combo;
}

void CurveBuilderWorkbench::refreshCurrencyCombo() {
    QStringList currencies;
    for (const auto& t : floatingIndexTypes_) {
        const auto dash = t.code.find('-');
        if (dash == std::string::npos)
            continue;
        const QString currency = QString::fromStdString(t.code.substr(0, dash));
        if (!currencies.contains(currency))
            currencies << currency;
    }
    currencies.sort();

    const QString previous = currencyCombo_->currentText();
    currencyCombo_->blockSignals(true);
    currencyCombo_->clear();
    currencyCombo_->addItems(currencies);
    currencyCombo_->setCurrentText(previous);
    currencyCombo_->blockSignals(false);

    if (imageCache_)
        setup_flag_combo(this, currencyCombo_, imageCache_, FlagSource::Currency);

    refreshIndexCombo();
}

void CurveBuilderWorkbench::refreshIndexCombo() {
    const QString currency = currencyCombo_->currentText();
    const QString previous = indexCombo_->currentText();

    QStringList codes;
    for (const auto& t : floatingIndexTypes_) {
        const QString code = QString::fromStdString(t.code);
        if (!currency.isEmpty() && !code.startsWith(currency + "-"))
            continue;
        codes << code;
    }
    codes.sort();

    indexCombo_->blockSignals(true);
    indexCombo_->clear();
    indexCombo_->addItems(codes);
    indexCombo_->setCurrentText(codes.contains(previous) ? previous : QString());
    indexCombo_->blockSignals(false);

    const bool hasIndex = !indexCombo_->currentText().isEmpty();
    browseSourceSeriesButton_->setEnabled(hasIndex);
    browseOutputSeriesButton_->setEnabled(hasIndex);
}

QString CurveBuilderWorkbench::selectedCurrency() const {
    return currencyCombo_->currentText();
}

QString CurveBuilderWorkbench::selectedIndexCode() const {
    return indexCombo_->currentText();
}

QString CurveBuilderWorkbench::selectedIndexQualifier() const {
    // floating_index_type codes use a dash after the currency (e.g. "USD-SOFR",
    // "EUR-EURIBOR-3M"), but market_series.qualifier for rates uses a slash there instead (e.g.
    // "USD/SOFR", "EUR/EURIBOR-3M" -- see ir_curve_feed.cpp's own qualifier construction). Only
    // the first dash is currency/family separator; any further dash (the tenor one) stays as-is.
    const QString code = selectedIndexCode();
    const int firstDash = code.indexOf('-');
    if (firstDash < 0)
        return code;
    return code.left(firstDash) + "/" + code.mid(firstDash + 1);
}

void CurveBuilderWorkbench::buildUi() {
    auto* layout = new QVBoxLayout(this);

    bannerLabel_ = new QLabel(this);
    bannerLabel_->setWordWrap(true);
    bannerLabel_->setVisible(false);
    layout->addWidget(bannerLabel_);

    auto* tabs = new QTabWidget(this);
    tabs->addTab(buildConventionsTab(), tr("Conventions"));
    tabs->addTab(buildPillarsTab(), tr("Pillars"));
    tabs->addTab(buildDiagnosticsTab(), tr("Build && Diagnostics"));
    layout->addWidget(tabs);
}

QWidget* CurveBuilderWorkbench::buildConventionsTab() {
    auto* page = new QWidget(this);
    auto* form = new QVBoxLayout(page);

    auto add_row = [&](const QString& label, QWidget* field) {
        auto* row = new QHBoxLayout();
        auto* l = new QLabel(label, page);
        l->setMinimumWidth(160);
        row->addWidget(l);
        row->addWidget(field, 1);
        form->addLayout(row);
    };

    auto add_picker_row = [&](const QString& label, QLineEdit* field, QPushButton*& browseButton) {
        field->setReadOnly(true);
        auto* row = new QHBoxLayout();
        auto* l = new QLabel(label, page);
        l->setMinimumWidth(160);
        row->addWidget(l);
        row->addWidget(field, 1);
        browseButton = new QPushButton(tr("Browse..."), page);
        row->addWidget(browseButton);
        form->addLayout(row);
    };

    // Currency/Index come first and gate everything below: picking these up front constrains
    // the Source/Output Series pickers to that index's series, instead of letting a novice
    // browse every FX/rates/credit series in the tenant unfiltered.
    currencyCombo_ = new QComboBox(page);
    currencyCombo_->setPlaceholderText(tr("Select..."));
    add_row(tr("Currency"), currencyCombo_);
    connect(currencyCombo_, &QComboBox::currentTextChanged, this, [this](const QString&) {
        refreshIndexCombo();
    });

    indexCombo_ = new QComboBox(page);
    indexCombo_->setPlaceholderText(tr("Select a currency first"));
    add_row(tr("Index"), indexCombo_);
    connect(indexCombo_, &QComboBox::currentTextChanged, this, [this](const QString& code) {
        const bool hasIndex = !code.isEmpty();
        browseSourceSeriesButton_->setEnabled(hasIndex);
        browseOutputSeriesButton_->setEnabled(hasIndex);
    });

    sourceSeriesIdEdit_ = new QLineEdit(page);
    sourceSeriesIdEdit_->setPlaceholderText(tr("Source (raw grid) market series"));
    add_picker_row(tr("Source Series"), sourceSeriesIdEdit_, browseSourceSeriesButton_);
    browseSourceSeriesButton_->setEnabled(false);
    connect(browseSourceSeriesButton_,
            &QPushButton::clicked,
            this,
            &CurveBuilderWorkbench::onBrowseSourceSeriesClicked);

    outputSeriesIdEdit_ = new QLineEdit(page);
    outputSeriesIdEdit_->setPlaceholderText(tr("Output (published curve) market series"));
    add_picker_row(tr("Output Series"), outputSeriesIdEdit_, browseOutputSeriesButton_);
    browseOutputSeriesButton_->setEnabled(false);
    connect(browseOutputSeriesButton_,
            &QPushButton::clicked,
            this,
            &CurveBuilderWorkbench::onBrowseOutputSeriesClicked);

    curveFamilyRoleCombo_ = new QComboBox(page);
    curveFamilyRoleCombo_->addItems({"FUNDING", "PROJECTION"});
    add_row(tr("Curve Family Role"), curveFamilyRoleCombo_);
    connect(
        curveFamilyRoleCombo_, &QComboBox::currentTextChanged, this, [this](const QString& role) {
            const bool isProjection = role == "PROJECTION";
            browseDiscountCurveConfigButton_->setEnabled(isProjection);
        });

    discountCurveConfigIdEdit_ = new QLineEdit(page);
    discountCurveConfigIdEdit_->setPlaceholderText(tr("Required for a Projection config"));
    add_picker_row(
        tr("Discount Curve Config"), discountCurveConfigIdEdit_, browseDiscountCurveConfigButton_);
    browseDiscountCurveConfigButton_->setEnabled(false);
    connect(browseDiscountCurveConfigButton_,
            &QPushButton::clicked,
            this,
            &CurveBuilderWorkbench::onBrowseDiscountCurveConfigClicked);

    interpolationMethodCombo_ = new QComboBox(page);
    interpolationMethodCombo_->addItems(
        {"LOG_LINEAR_DISCOUNT", "FLAT_FORWARD_THEN_LOG_LINEAR", "CUBIC_SPLINE"});
    add_row(tr("Interpolation Method"), interpolationMethodCombo_);

    dayCountConventionCombo_ = new QComboBox(page);
    dayCountConventionCombo_->addItems({"A360", "A365", "A365F", "30/360"});
    add_row(tr("Day Count Convention"), dayCountConventionCombo_);

    splitTenorCodeEdit_ = new QLineEdit(page);
    splitTenorCodeEdit_->setPlaceholderText(tr("e.g. 2Y -- the curve's own last pillar tenor for "
                                               "a single-segment interpolation method"));
    add_row(tr("Split Tenor Code"), splitTenorCodeEdit_);

    conventionsErrorLabel_ = new QLabel(page);
    conventionsErrorLabel_->setStyleSheet("color: #f87171;");
    conventionsErrorLabel_->setWordWrap(true);
    conventionsErrorLabel_->setVisible(false);
    form->addWidget(conventionsErrorLabel_);

    form->addStretch();
    return page;
}

QWidget* CurveBuilderWorkbench::buildPillarsTab() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);

    auto* explainerLabel = new QLabel(
        tr("Each row is one instrument's date range, not a single point: Start Tenor is when its "
           "rate period begins (relative to the As Of date), End Tenor is when it matures. A "
           "1-month deposit starting today is Start Tenor SPOT, End Tenor 1M; a 3-month deposit "
           "starting today is Start Tenor SPOT, End Tenor 3M -- both share the same start (spot) "
           "but differ in maturity, standard money-market convention. Both fields are seeded "
           "tenor codes (Reference Data > Curve Building > Tenors), not free text."),
        page);
    explainerLabel->setWordWrap(true);
    explainerLabel->setStyleSheet("color: #94a3b8; font-style: italic;");
    layout->addWidget(explainerLabel);

    pillarsTable_ = new QTableWidget(0, 3, page);
    pillarsTable_->setHorizontalHeaderLabels(
        {tr("Start Tenor"), tr("End Tenor"), tr("Curve Role")});
    pillarsTable_->horizontalHeader()->setStretchLastSection(true);
    layout->addWidget(pillarsTable_);

    auto* buttonsRow = new QHBoxLayout();
    addPillarButton_ = new QPushButton(tr("Add Pillar"), page);
    addPillarButton_->setEnabled(false);
    addPillarButton_->setToolTip(tr("Loading tenor codes..."));
    auto* removeButton = new QPushButton(tr("Remove Selected"), page);
    connect(
        addPillarButton_, &QPushButton::clicked, this, &CurveBuilderWorkbench::onAddPillarClicked);
    connect(
        removeButton, &QPushButton::clicked, this, &CurveBuilderWorkbench::onRemovePillarClicked);
    buttonsRow->addWidget(addPillarButton_);
    buttonsRow->addWidget(removeButton);
    buttonsRow->addStretch();
    layout->addLayout(buttonsRow);

    return page;
}

QWidget* CurveBuilderWorkbench::buildDiagnosticsTab() {
    auto* page = new QWidget(this);
    auto* layout = new QVBoxLayout(page);

    auto* topRow = new QHBoxLayout();
    topRow->addWidget(new QLabel(tr("As of:"), page));
    asOfEdit_ = new QDateTimeEdit(QDateTime::currentDateTimeUtc(), page);
    asOfEdit_->setCalendarPopup(true);
    topRow->addWidget(asOfEdit_);
    topRow->addStretch();

    saveButton_ = new QPushButton(tr("Save"), page);
    bootstrapButton_ = new QPushButton(tr("Bootstrap Curve"), page);
    publishButton_ = new QPushButton(tr("Publish"), page);
    connect(saveButton_, &QPushButton::clicked, this, &CurveBuilderWorkbench::onSaveClicked);
    connect(
        bootstrapButton_, &QPushButton::clicked, this, &CurveBuilderWorkbench::onBootstrapClicked);
    connect(publishButton_, &QPushButton::clicked, this, &CurveBuilderWorkbench::onPublishClicked);
    topRow->addWidget(saveButton_);
    topRow->addWidget(bootstrapButton_);
    topRow->addWidget(publishButton_);
    layout->addLayout(topRow);

    bootstrapHintLabel_ =
        new QLabel(tr("Save the recipe with at least one pillar before bootstrapping."), page);
    bootstrapHintLabel_->setStyleSheet("color: #94a3b8; font-style: italic;");
    layout->addWidget(bootstrapHintLabel_);

    publishHintLabel_ = new QLabel(tr("Bootstrap the curve before publishing."), page);
    publishHintLabel_->setStyleSheet("color: #94a3b8; font-style: italic;");
    layout->addWidget(publishHintLabel_);

    resultsTable_ = new QTableWidget(0, 3, page);
    resultsTable_->setHorizontalHeaderLabels({tr("Tenor"), tr("Date"), tr("Discount Factor")});
    resultsTable_->horizontalHeader()->setStretchLastSection(true);
    resultsTable_->setMaximumHeight(180);
    layout->addWidget(resultsTable_);

    dfChart_ = make_chart(tr("Discount Factors"));
    dfChartView_ = new QChartView(dfChart_, page);
    dfChartView_->setRenderHint(QPainter::Antialiasing);
    layout->addWidget(dfChartView_);

    forwardChart_ = make_chart(tr("Forward Rate Health Check"));
    forwardChartView_ = new QChartView(forwardChart_, page);
    forwardChartView_->setRenderHint(QPainter::Antialiasing);
    layout->addWidget(forwardChartView_);

    healthFindingsList_ = new QListWidget(page);
    healthFindingsList_->setMaximumHeight(120);
    layout->addWidget(healthFindingsList_);

    return page;
}

void CurveBuilderWorkbench::setCreateMode(bool createMode) {
    createMode_ = createMode;
    if (createMode_) {
        config_ = refdata::domain::ir_curve_bootstrap_config{};
        config_.id = boost::uuids::random_generator{}();
        pillars_.clear();
    }
    loadConfigIntoUi();
    loadPillarsIntoTable();
    updateActionStates();
}

void CurveBuilderWorkbench::setConfig(const refdata::domain::ir_curve_bootstrap_config& config) {
    createMode_ = false;
    config_ = config;
    loadConfigIntoUi();
    updateActionStates();
}

QString CurveBuilderWorkbench::code() const {
    return QString::fromStdString(boost::uuids::to_string(config_.id));
}

void CurveBuilderWorkbench::loadConfigIntoUi() {
    // Existing records show the raw id (no cached friendly label to display without an extra
    // fetch); a fresh pick via Browse... below sets a friendly "type / metric / qualifier" label
    // instead. Either way the id itself lives only in config_, never re-parsed from this text.
    const auto nilSourceId = boost::uuids::nil_uuid();
    sourceSeriesIdEdit_->setText(config_.source_series_id == nilSourceId ?
                                     QString() :
                                     QString::fromStdString(
                                         boost::uuids::to_string(config_.source_series_id)));
    outputSeriesIdEdit_->setText(config_.output_series_id == nilSourceId ?
                                     QString() :
                                     QString::fromStdString(
                                         boost::uuids::to_string(config_.output_series_id)));
    curveFamilyRoleCombo_->setCurrentText(QString::fromStdString(
        config_.curve_family_role.empty() ? "FUNDING" : config_.curve_family_role));
    discountCurveConfigIdEdit_->setText(
        config_.discount_curve_config_id == nilSourceId ?
            QString() :
            QString::fromStdString(boost::uuids::to_string(config_.discount_curve_config_id)));
    browseDiscountCurveConfigButton_->setEnabled(config_.curve_family_role == "PROJECTION");
    interpolationMethodCombo_->setCurrentText(QString::fromStdString(
        config_.interpolation_method.empty() ? "LOG_LINEAR_DISCOUNT" :
                                               config_.interpolation_method));
    if (!config_.day_count_convention.empty())
        dayCountConventionCombo_->setCurrentText(
            QString::fromStdString(config_.day_count_convention));
    splitTenorCodeEdit_->setText(QString::fromStdString(config_.split_tenor_code));
}

void CurveBuilderWorkbench::loadPillarsIntoTable() {
    pillarsTable_->setRowCount(0);
    for (const auto& p : pillars_) {
        const int row = pillarsTable_->rowCount();
        pillarsTable_->insertRow(row);
        pillarsTable_->setCellWidget(
            row, 0, makeTenorCombo(QString::fromStdString(p.start_tenor_code)));
        pillarsTable_->setCellWidget(
            row, 1, makeTenorCombo(QString::fromStdString(p.end_tenor_code)));
        pillarsTable_->setCellWidget(
            row, 2, makeCurveRoleCombo(QString::fromStdString(p.curve_role_code)));
    }
}

void CurveBuilderWorkbench::onAddPillarClicked() {
    const int row = pillarsTable_->rowCount();
    pillarsTable_->insertRow(row);
    // "SPOT" is the real seeded spot-starting tenor code (Reference Data > Curve Building >
    // Tenors) -- both columns are combos over that same table now, so an invalid code like the
    // free-text era's "0D" (which curve_republish_resolver rejected) can no longer be entered.
    pillarsTable_->setCellWidget(row, 0, makeTenorCombo("SPOT"));
    pillarsTable_->setCellWidget(row, 1, makeTenorCombo(QString()));
    pillarsTable_->setCellWidget(row, 2, makeCurveRoleCombo("DEPOSIT"));
}

void CurveBuilderWorkbench::onRemovePillarClicked() {
    const auto rows = pillarsTable_->selectionModel()->selectedRows();
    QList<int> rowNumbers;
    for (const auto& idx : rows)
        rowNumbers.push_back(idx.row());
    std::sort(rowNumbers.rbegin(), rowNumbers.rend());
    for (int r : rowNumbers)
        pillarsTable_->removeRow(r);
}

void CurveBuilderWorkbench::onBrowseSourceSeriesClicked() {
    // Source is the raw, externally-fed grid -- RATES/YIELD is the convention every seeded raw
    // rates row actually uses.
    MarketSeriesPickerDialog::Options options;
    options.initialFilter = selectedIndexQualifier();
    MarketSeriesPickerDialog dialog(clientManager_, this, options);
    if (dialog.exec() != QDialog::Accepted)
        return;
    const auto series = dialog.selectedSeries();
    if (!series)
        return;
    config_.source_series_id = series->id;
    sourceSeriesIdEdit_->setText(QString::fromStdString(
        series->series_type + " / " + series->metric + " / " + series->qualifier));
}

void CurveBuilderWorkbench::onBrowseOutputSeriesClicked() {
    // Output is the published curve, not the raw grid -- defaults to DISCOUNT/RATE (the
    // published-curve convention per the oresmd design doc's own worked examples) rather than
    // RATES/YIELD, so a freshly-created output series is never mistakable for its own raw input.
    // The already-picked Source series is excluded outright: the two must never resolve to the
    // same market_series row -- see ir_curve_bootstrap_config_service::save_bootstrap_config()'s
    // matching server-side guard for why.
    MarketSeriesPickerDialog::Options options;
    options.initialFilter = selectedIndexQualifier();
    options.defaultSeriesType = "DISCOUNT";
    options.defaultMetric = "RATE";
    if (!(config_.source_series_id == boost::uuids::nil_uuid()))
        options.excludeSeriesId =
            QString::fromStdString(boost::uuids::to_string(config_.source_series_id));
    MarketSeriesPickerDialog dialog(clientManager_, this, options);
    if (dialog.exec() != QDialog::Accepted)
        return;
    const auto series = dialog.selectedSeries();
    if (!series)
        return;
    if (series->id == config_.source_series_id) {
        showBanner(tr("Output series cannot be the same as the Source series."), true);
        return;
    }
    config_.output_series_id = series->id;
    outputSeriesIdEdit_->setText(QString::fromStdString(
        series->series_type + " / " + series->metric + " / " + series->qualifier));
}

void CurveBuilderWorkbench::onBrowseDiscountCurveConfigClicked() {
    BootstrapConfigPickerDialog dialog(clientManager_, this, selectedCurrency());
    if (dialog.exec() != QDialog::Accepted)
        return;
    const auto picked = dialog.selectedConfig();
    if (!picked)
        return;
    config_.discount_curve_config_id = picked->id;
    discountCurveConfigIdEdit_->setText(
        QString("Config %1 (%2)")
            .arg(QString::fromStdString(boost::uuids::to_string(picked->id)).left(8))
            .arg(QString::fromStdString(picked->interpolation_method)));
}

void CurveBuilderWorkbench::collectConfigFromUi() {
    // source_series_id/output_series_id/discount_curve_config_id are set directly by the
    // Browse... picker slots (onBrowseSourceSeriesClicked etc.), not parsed from the display
    // text here -- the text is a friendly label, not necessarily a valid UUID string.
    config_.curve_family_role = curveFamilyRoleCombo_->currentText().toStdString();
    if (config_.curve_family_role != "PROJECTION")
        config_.discount_curve_config_id = boost::uuids::nil_uuid();
    config_.interpolation_method = interpolationMethodCombo_->currentText().toStdString();
    config_.day_count_convention = dayCountConventionCombo_->currentText().toStdString();
    config_.split_tenor_code = splitTenorCodeEdit_->text().toStdString();
    config_.modified_by = username_;
    config_.performed_by = username_;
    config_.change_reason_code = pendingChangeReasonCode_;
    config_.change_commentary = pendingChangeCommentary_;
}

void CurveBuilderWorkbench::collectPillarsFromTable() {
    std::vector<refdata::domain::ir_curve_bootstrap_pillar> collected;
    for (int row = 0; row < pillarsTable_->rowCount(); ++row) {
        refdata::domain::ir_curve_bootstrap_pillar p;
        p.id = row < static_cast<int>(pillars_.size()) ?
                   pillars_[static_cast<std::size_t>(row)].id :
                   boost::uuids::random_generator{}();
        p.bootstrap_config_id = config_.id;
        p.party_id = config_.party_id;
        p.sequence_index = row;
        auto combo_text = [&](int col) -> std::string {
            auto* combo = qobject_cast<QComboBox*>(pillarsTable_->cellWidget(row, col));
            return combo ? combo->currentText().toStdString() : "";
        };
        p.start_tenor_code = combo_text(0);
        p.end_tenor_code = combo_text(1);
        p.curve_role_code = combo_text(2);
        p.modified_by = username_;
        p.performed_by = username_;
        p.change_reason_code = pendingChangeReasonCode_;
        p.change_commentary = pendingChangeCommentary_;
        collected.push_back(std::move(p));
    }
    pillars_ = std::move(collected);
}

void CurveBuilderWorkbench::updateActionStates() {
    const bool hasRecipe = !createMode_ || !pillars_.empty();
    bootstrapButton_->setEnabled(hasRecipe);
    bootstrapHintLabel_->setVisible(!hasRecipe);
    publishButton_->setEnabled(hasBootstrapped_);
    publishHintLabel_->setVisible(!hasBootstrapped_);
}

void CurveBuilderWorkbench::showBanner(const QString& message, bool isError) {
    bannerLabel_->setText(message);
    bannerLabel_->setStyleSheet(isError ? "color: #f87171; font-weight: bold;" :
                                          "color: #4ade80; font-weight: bold;");
    bannerLabel_->setVisible(!message.isEmpty());
}

void CurveBuilderWorkbench::showError(const QString& title,
                                      const QString& shortMessage,
                                      const QString& details) {
    showBanner(shortMessage, true);
    MessageBoxHelper::critical(this, title, shortMessage, details);
}

bool CurveBuilderWorkbench::promptAndStashChangeReason() {
    if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
        showBanner(tr("Change reasons not loaded. Please try again."), true);
        return false;
    }
    const auto opType =
        createMode_ ? ChangeReasonDialog::OperationType::Create :
                      ChangeReasonDialog::OperationType::Amend;
    const std::string category = createMode_ ? "system" : "common";
    std::vector<dq::domain::change_reason> reasons;
    switch (opType) {
        case ChangeReasonDialog::OperationType::Create:
            reasons = changeReasonCache_->getReasonsForNew(category);
            break;
        case ChangeReasonDialog::OperationType::Amend:
            reasons = changeReasonCache_->getReasonsForAmend(category);
            break;
        case ChangeReasonDialog::OperationType::Delete:
            reasons = changeReasonCache_->getReasonsForDelete(category);
            break;
    }
    if (reasons.empty()) {
        showBanner(tr("No change reasons available. Please contact administrator."), true);
        return false;
    }

    // isDirty is always true here (Save always writes) -- unlike a save-and-close dialog this
    // workbench has no separate "touch with no field changes" path to disable
    // common.non_material_update for.
    ChangeReasonDialog dlg(reasons, opType, /*hasFieldChanges=*/true, this);
    if (dlg.exec() != QDialog::Accepted)
        return false;
    pendingChangeReasonCode_ = dlg.selectedReasonCode();
    pendingChangeCommentary_ = dlg.commentary();
    return true;
}

void CurveBuilderWorkbench::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        showBanner(tr("Not connected to server."), true);
        return;
    }
    collectConfigFromUi();
    collectPillarsFromTable();

    if (pillars_.empty()) {
        showBanner(tr("Add at least one pillar before saving."), true);
        return;
    }
    // curve_bootstrap_engine anchors every DEPOSIT pillar's rate at df_start == 1.0 (the
    // valuation date itself, per curve_instrument_pricer::deposit_rate's own contract) -- a
    // DEPOSIT pillar whose Start Tenor isn't SPOT (e.g. a forward-starting 3M-into-6M pillar)
    // fails at Bootstrap time with a bare "start_date == value_date" exception. Catch the
    // mismatch here instead: a forward-starting pillar is an FRA or SWAP, not a DEPOSIT.
    for (const auto& p : pillars_) {
        if (p.curve_role_code == "DEPOSIT" && p.start_tenor_code != "SPOT") {
            showBanner(tr("Pillar '%1' is a DEPOSIT starting at %2, not SPOT -- DEPOSIT pillars "
                          "must start today. For a forward-starting instrument, change its Curve "
                          "Role to FRA or SWAP instead.")
                          .arg(QString::fromStdString(p.end_tenor_code))
                          .arg(QString::fromStdString(p.start_tenor_code)),
                      true);
            return;
        }
    }
    if (config_.split_tenor_code.empty()) {
        // split_tenor_code is never optional (see the table's own DB check constraint) -- for a
        // single-segment interpolation method it is, per that table's doc comment, a genuine
        // value equal to the curve's own last pillar's end_tenor_code, not a fabricated
        // sentinel, so it can be derived automatically rather than making the user type a
        // duplicate of data already on the Pillars tab. FLAT_FORWARD_THEN_LOG_LINEAR's split is
        // a genuine independent choice (the knot between its two segments), so it still requires
        // an explicit value.
        if (config_.interpolation_method != "FLAT_FORWARD_THEN_LOG_LINEAR") {
            config_.split_tenor_code = pillars_.back().end_tenor_code;
            splitTenorCodeEdit_->setText(QString::fromStdString(config_.split_tenor_code));
        }
        if (config_.split_tenor_code.empty()) {
            showBanner(tr("Split Tenor Code is required -- for %1, enter the curve's own split "
                          "point (the knot tenor between its two interpolation segments).")
                          .arg(QString::fromStdString(config_.interpolation_method)),
                      true);
            return;
        }
    }
    if (!(config_.source_series_id == boost::uuids::nil_uuid()) &&
        config_.source_series_id == config_.output_series_id) {
        showBanner(tr("Source and Output series must be different (server also rejects this)."),
                  true);
        return;
    }
    if (!promptAndStashChangeReason())
        return;
    // Re-collect: pendingChangeReasonCode_/pendingChangeCommentary_ were just set by the prompt
    // above, and both collect functions stamp them onto config_/each pillar.
    collectConfigFromUi();
    collectPillarsFromTable();

    saveButton_->setEnabled(false);
    const auto configRequest =
        refdata::messaging::save_ir_curve_bootstrap_config_request::from(config_);
    const auto pillarsCopy = pillars_;

    QPointer<CurveBuilderWorkbench> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager, configRequest, pillarsCopy]() -> std::expected<QString, QString> {
            auto configResult = clientManager->process_authenticated_request(configRequest);
            if (!configResult)
                return std::unexpected(QString::fromStdString(configResult.error()));
            if (!configResult->success)
                return std::unexpected(QString::fromStdString(configResult->message));

            for (const auto& p : pillarsCopy) {
                auto pillarRequest =
                    refdata::messaging::save_ir_curve_bootstrap_pillar_request::from(p);
                auto pillarResult = clientManager->process_authenticated_request(pillarRequest);
                if (!pillarResult)
                    return std::unexpected(QString::fromStdString(pillarResult.error()));
                if (!pillarResult->success)
                    return std::unexpected(QString::fromStdString(pillarResult->message));
            }
            return QString("Saved");
        });

    auto* watcher = new QFutureWatcher<std::expected<QString, QString>>(this);
    connect(watcher,
            &QFutureWatcher<std::expected<QString, QString>>::finished,
            this,
            [self, watcher]() {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                self->saveButton_->setEnabled(true);
                if (!result) {
                    self->showError(tr("Save Failed"),
                                    tr("Failed to save the curve bootstrap recipe."),
                                    result.error());
                    return;
                }
                self->createMode_ = false;
                self->showBanner(tr("Recipe saved."), false);
                self->updateActionStates();
                emit self->configSaved(self->code());
            });
    watcher->setFuture(future);
}

void CurveBuilderWorkbench::onBootstrapClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        showBanner(tr("Not connected to server."), true);
        return;
    }

    marketdata::messaging::compute_curve_request request;
    request.bootstrap_config_id = boost::uuids::to_string(config_.id);
    request.as_of = to_time_point(asOfEdit_->dateTime());

    bootstrapButton_->setEnabled(false);
    QPointer<CurveBuilderWorkbench> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager,
         request]() -> std::expected<marketdata::messaging::compute_curve_response, QString> {
            auto result = clientManager->process_authenticated_request(request);
            if (!result)
                return std::unexpected(QString::fromStdString(result.error()));
            if (!result->success)
                return std::unexpected(QString::fromStdString(result->message));
            return *result;
        });

    using ResultType = std::expected<marketdata::messaging::compute_curve_response, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        self->bootstrapButton_->setEnabled(true);
        if (!result) {
            self->showError(
                tr("Bootstrap Failed"), tr("Failed to bootstrap the curve."), result.error());
            self->hasBootstrapped_ = false;
            self->updateActionStates();
            return;
        }
        self->showBanner(
            tr("Bootstrap succeeded -- %1 point(s) computed.").arg(result->points.size()), false);
        self->hasBootstrapped_ = true;
        self->renderBootstrapResults(result->points);
        self->updateActionStates();
    });
    watcher->setFuture(future);
}

void CurveBuilderWorkbench::onPublishClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        showBanner(tr("Not connected to server."), true);
        return;
    }

    marketdata::messaging::republish_curve_request request;
    request.bootstrap_config_id = boost::uuids::to_string(config_.id);
    request.as_of = to_time_point(asOfEdit_->dateTime());

    publishButton_->setEnabled(false);
    QPointer<CurveBuilderWorkbench> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request]() -> std::expected<QString, QString> {
        auto result = clientManager->process_authenticated_request(request);
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return QString("Published");
    });

    auto* watcher = new QFutureWatcher<std::expected<QString, QString>>(this);
    connect(watcher,
            &QFutureWatcher<std::expected<QString, QString>>::finished,
            this,
            [self, watcher]() {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                self->publishButton_->setEnabled(true);
                self->updateActionStates();
                if (!result) {
                    self->showError(
                        tr("Publish Failed"), tr("Failed to publish the curve."), result.error());
                    return;
                }
                self->showBanner(tr("Curve published."), false);
            });
    watcher->setFuture(future);
}

void CurveBuilderWorkbench::renderBootstrapResults(
    const std::vector<marketdata::messaging::computed_curve_point>& points) {
    namespace quant = ores::analytics::quant::service;

    resultsTable_->setRowCount(0);
    QStringList tenorLabels;
    auto* dfSeries = new QLineSeries();
    std::vector<quant::bootstrapped_point> bootstrapped;
    bootstrapped.reserve(points.size());

    for (std::size_t i = 0; i < points.size(); ++i) {
        const auto& p = points[i];
        const int row = resultsTable_->rowCount();
        resultsTable_->insertRow(row);
        resultsTable_->setItem(row, 0, new QTableWidgetItem(QString::fromStdString(p.point_id)));
        resultsTable_->setItem(row, 1, new QTableWidgetItem(to_qstring(p.date)));
        resultsTable_->setItem(
            row, 2, new QTableWidgetItem(QString::number(p.discount_factor, 'f', 6)));

        tenorLabels << QString::fromStdString(p.point_id);
        dfSeries->append(static_cast<double>(i), p.discount_factor);
        bootstrapped.push_back({p.point_id, p.date, p.discount_factor});
    }

    dfChart_->removeAllSeries();
    for (auto* axis : dfChart_->axes())
        dfChart_->removeAxis(axis);
    dfChart_->addSeries(dfSeries);
    auto* dfAxisX = new QBarCategoryAxis();
    dfAxisX->append(tenorLabels);
    auto* dfAxisY = new QValueAxis();
    dfChart_->addAxis(dfAxisX, Qt::AlignBottom);
    dfChart_->addAxis(dfAxisY, Qt::AlignLeft);
    dfSeries->attachAxis(dfAxisX);
    dfSeries->attachAxis(dfAxisY);
    dfChart_->legend()->hide();

    healthFindingsList_->clear();

    quant::day_count_convention_code dayCountConvention;
    try {
        dayCountConvention = quant::parse_day_count_convention_code(config_.day_count_convention);
    } catch (const std::exception& e) {
        healthFindingsList_->addItem(
            QString("Could not run diagnostics: %1").arg(QString::fromStdString(e.what())));
        return;
    }

    const auto dfFindings = quant::curve_health_checker::check_discount_factors(bootstrapped);
    for (const auto& f : dfFindings)
        healthFindingsList_->addItem(QString::fromStdString(f.point_id + ": " + f.message));

    const auto forwards =
        quant::forward_rate_calculator::calculate(bootstrapped, dayCountConvention);
    auto* forwardSeries = new QLineSeries();
    QStringList forwardLabels;
    for (std::size_t i = 0; i < forwards.size(); ++i) {
        forwardSeries->append(static_cast<double>(i), forwards[i].instantaneous_forward_rate);
        forwardLabels << QString::fromStdString(forwards[i].point_id);
    }

    forwardChart_->removeAllSeries();
    for (auto* axis : forwardChart_->axes())
        forwardChart_->removeAxis(axis);
    forwardChart_->addSeries(forwardSeries);
    auto* fwdAxisX = new QBarCategoryAxis();
    fwdAxisX->append(forwardLabels);
    auto* fwdAxisY = new QValueAxis();
    forwardChart_->addAxis(fwdAxisX, Qt::AlignBottom);
    forwardChart_->addAxis(fwdAxisY, Qt::AlignLeft);
    forwardSeries->attachAxis(fwdAxisX);
    forwardSeries->attachAxis(fwdAxisY);
    forwardChart_->legend()->hide();

    const auto forwardFindings = quant::curve_health_checker::check_forward_rates(forwards);
    for (const auto& f : forwardFindings)
        healthFindingsList_->addItem(QString::fromStdString(f.point_id + ": " + f.message));
}

}
