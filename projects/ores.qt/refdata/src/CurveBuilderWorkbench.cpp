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
#include "ores.qt/ClientManager.hpp"
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

    sourceSeriesIdEdit_ = new QLineEdit(page);
    sourceSeriesIdEdit_->setPlaceholderText(tr("Source (raw grid) market series id"));
    add_row(tr("Source Series Id"), sourceSeriesIdEdit_);

    outputSeriesIdEdit_ = new QLineEdit(page);
    outputSeriesIdEdit_->setPlaceholderText(tr("Output (published curve) market series id"));
    add_row(tr("Output Series Id"), outputSeriesIdEdit_);

    curveFamilyRoleCombo_ = new QComboBox(page);
    curveFamilyRoleCombo_->addItems({"FUNDING", "PROJECTION"});
    add_row(tr("Curve Family Role"), curveFamilyRoleCombo_);
    connect(
        curveFamilyRoleCombo_, &QComboBox::currentTextChanged, this, [this](const QString& role) {
            discountCurveConfigIdEdit_->setEnabled(role == "PROJECTION");
        });

    discountCurveConfigIdEdit_ = new QLineEdit(page);
    discountCurveConfigIdEdit_->setPlaceholderText(tr("Required for a Projection config"));
    discountCurveConfigIdEdit_->setEnabled(false);
    add_row(tr("Discount Curve Config Id"), discountCurveConfigIdEdit_);

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

    pillarsTable_ = new QTableWidget(0, 3, page);
    pillarsTable_->setHorizontalHeaderLabels(
        {tr("Start Tenor"), tr("End Tenor"), tr("Curve Role")});
    pillarsTable_->horizontalHeader()->setStretchLastSection(true);
    layout->addWidget(pillarsTable_);

    auto* buttonsRow = new QHBoxLayout();
    auto* addButton = new QPushButton(tr("Add Pillar"), page);
    auto* removeButton = new QPushButton(tr("Remove Selected"), page);
    connect(addButton, &QPushButton::clicked, this, &CurveBuilderWorkbench::onAddPillarClicked);
    connect(
        removeButton, &QPushButton::clicked, this, &CurveBuilderWorkbench::onRemovePillarClicked);
    buttonsRow->addWidget(addButton);
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
    sourceSeriesIdEdit_->setText(
        QString::fromStdString(boost::uuids::to_string(config_.source_series_id)));
    outputSeriesIdEdit_->setText(
        QString::fromStdString(boost::uuids::to_string(config_.output_series_id)));
    curveFamilyRoleCombo_->setCurrentText(QString::fromStdString(
        config_.curve_family_role.empty() ? "FUNDING" : config_.curve_family_role));
    discountCurveConfigIdEdit_->setText(
        QString::fromStdString(boost::uuids::to_string(config_.discount_curve_config_id)));
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
        pillarsTable_->setItem(
            row, 0, new QTableWidgetItem(QString::fromStdString(p.start_tenor_code)));
        pillarsTable_->setItem(
            row, 1, new QTableWidgetItem(QString::fromStdString(p.end_tenor_code)));
        pillarsTable_->setItem(
            row, 2, new QTableWidgetItem(QString::fromStdString(p.curve_role_code)));
    }
}

void CurveBuilderWorkbench::onAddPillarClicked() {
    const int row = pillarsTable_->rowCount();
    pillarsTable_->insertRow(row);
    pillarsTable_->setItem(row, 0, new QTableWidgetItem("SPOT"));
    pillarsTable_->setItem(row, 1, new QTableWidgetItem(""));
    pillarsTable_->setItem(row, 2, new QTableWidgetItem("DEPOSIT"));
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

void CurveBuilderWorkbench::collectConfigFromUi() {
    try {
        config_.source_series_id =
            boost::lexical_cast<boost::uuids::uuid>(sourceSeriesIdEdit_->text().toStdString());
    } catch (const std::exception&) {
        config_.source_series_id = boost::uuids::nil_uuid();
    }
    try {
        config_.output_series_id =
            boost::lexical_cast<boost::uuids::uuid>(outputSeriesIdEdit_->text().toStdString());
    } catch (const std::exception&) {
        config_.output_series_id = boost::uuids::nil_uuid();
    }
    config_.curve_family_role = curveFamilyRoleCombo_->currentText().toStdString();
    if (config_.curve_family_role == "PROJECTION") {
        try {
            config_.discount_curve_config_id = boost::lexical_cast<boost::uuids::uuid>(
                discountCurveConfigIdEdit_->text().toStdString());
        } catch (const std::exception&) {
            config_.discount_curve_config_id = boost::uuids::nil_uuid();
        }
    } else {
        config_.discount_curve_config_id = boost::uuids::nil_uuid();
    }
    config_.interpolation_method = interpolationMethodCombo_->currentText().toStdString();
    config_.day_count_convention = dayCountConventionCombo_->currentText().toStdString();
    config_.split_tenor_code = splitTenorCodeEdit_->text().toStdString();
    config_.modified_by = username_;
    config_.performed_by = username_;
    config_.change_reason_code = "system.curve_builder_workbench";
    config_.change_commentary =
        createMode_ ? "Created via Curve Builder Workbench" : "Edited via Curve Builder Workbench";
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
        p.start_tenor_code =
            pillarsTable_->item(row, 0) ? pillarsTable_->item(row, 0)->text().toStdString() : "";
        p.end_tenor_code =
            pillarsTable_->item(row, 1) ? pillarsTable_->item(row, 1)->text().toStdString() : "";
        p.curve_role_code =
            pillarsTable_->item(row, 2) ? pillarsTable_->item(row, 2)->text().toStdString() : "";
        p.modified_by = username_;
        p.performed_by = username_;
        p.change_reason_code = "system.curve_builder_workbench";
        p.change_commentary = "Edited via Curve Builder Workbench";
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
                    self->showBanner(tr("Save failed: %1").arg(result.error()), true);
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
            self->showBanner(tr("Bootstrap failed: %1").arg(result.error()), true);
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
                    self->showBanner(tr("Publish failed: %1").arg(result.error()), true);
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
