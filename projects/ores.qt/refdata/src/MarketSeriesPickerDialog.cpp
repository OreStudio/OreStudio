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
#include "ores.qt/MarketSeriesPickerDialog.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.qt/ClientManager.hpp"
#include <QAbstractItemView>
#include <QComboBox>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QPointer>
#include <QPushButton>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/enums.hpp>

namespace ores::qt {

namespace {
namespace md = ores::marketdata;

const QStringList& asset_class_names() {
    static const QStringList names = {
        "fx", "rates", "credit", "equity", "commodity", "inflation", "bond", "cross_asset"};
    return names;
}

const QStringList& subclass_names() {
    static const QStringList names = {"spot",
                                      "forward",
                                      "volatility",
                                      "yield",
                                      "basis",
                                      "fra",
                                      "xccy",
                                      "spread",
                                      "index_credit",
                                      "recovery",
                                      "swap",
                                      "capfloor",
                                      "seasonality",
                                      "price",
                                      "correlation"};
    return names;
}

}

MarketSeriesPickerDialog::MarketSeriesPickerDialog(ClientManager* clientManager, QWidget* parent)
    : QDialog(parent)
    , clientManager_(clientManager) {
    setWindowTitle(tr("Select Market Series"));
    resize(720, 480);

    auto* layout = new QVBoxLayout(this);

    filterEdit_ = new QLineEdit(this);
    filterEdit_->setPlaceholderText(tr("Filter by type, metric or qualifier..."));
    connect(filterEdit_, &QLineEdit::textChanged, this, &MarketSeriesPickerDialog::populateTable);
    layout->addWidget(filterEdit_);

    table_ = new QTableWidget(0, 4, this);
    table_->setHorizontalHeaderLabels(
        {tr("Series Type"), tr("Metric"), tr("Qualifier"), tr("Subclass")});
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    connect(table_, &QTableWidget::cellDoubleClicked, this, [this](int row, int) {
        onRowActivated(row);
    });
    layout->addWidget(table_);

    auto* buttonsRow = new QHBoxLayout();
    selectButton_ = new QPushButton(tr("Select"), this);
    connect(
        selectButton_, &QPushButton::clicked, this, &MarketSeriesPickerDialog::onSelectClicked);
    auto* cancelButton = new QPushButton(tr("Cancel"), this);
    connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
    buttonsRow->addStretch();
    buttonsRow->addWidget(selectButton_);
    buttonsRow->addWidget(cancelButton);
    layout->addLayout(buttonsRow);

    layout->addWidget(buildCreatePanel());

    statusLabel_ = new QLabel(this);
    statusLabel_->setStyleSheet("color: #f87171;");
    statusLabel_->setWordWrap(true);
    statusLabel_->setVisible(false);
    layout->addWidget(statusLabel_);

    reload();
}

QWidget* MarketSeriesPickerDialog::buildCreatePanel() {
    auto* panel = new QWidget(this);
    auto* outer = new QVBoxLayout(panel);
    outer->setContentsMargins(0, 8, 0, 0);
    outer->addWidget(new QLabel(tr("New series (when the one you need doesn't exist yet):"),
                                panel));

    auto* row = new QHBoxLayout();
    newSeriesTypeEdit_ = new QLineEdit(panel);
    newSeriesTypeEdit_->setPlaceholderText(tr("Series Type, e.g. YieldCurve"));
    newMetricEdit_ = new QLineEdit(panel);
    newMetricEdit_->setPlaceholderText(tr("Metric, e.g. DISCOUNT"));
    newQualifierEdit_ = new QLineEdit(panel);
    newQualifierEdit_->setPlaceholderText(tr("Qualifier, e.g. EUR-EONIA"));
    newAssetClassCombo_ = new QComboBox(panel);
    newAssetClassCombo_->addItems(asset_class_names());
    newAssetClassCombo_->setCurrentText("rates");
    newSubclassCombo_ = new QComboBox(panel);
    newSubclassCombo_->addItems(subclass_names());
    newSubclassCombo_->setCurrentText("yield");
    row->addWidget(newSeriesTypeEdit_);
    row->addWidget(newMetricEdit_);
    row->addWidget(newQualifierEdit_);
    row->addWidget(newAssetClassCombo_);
    row->addWidget(newSubclassCombo_);
    auto* createButton = new QPushButton(tr("Create && Select"), panel);
    connect(createButton, &QPushButton::clicked, this, &MarketSeriesPickerDialog::onCreateClicked);
    row->addWidget(createButton);
    outer->addLayout(row);

    return panel;
}

QString MarketSeriesPickerDialog::display_label(const marketdata::domain::market_series& s) {
    return QString::fromStdString(s.series_type + " / " + s.metric + " / " + s.qualifier);
}

void MarketSeriesPickerDialog::reload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        statusLabel_->setText(tr("Not connected to server."));
        statusLabel_->setVisible(true);
        return;
    }

    md::messaging::get_market_series_request request;
    request.offset = 0;
    request.limit = 1000;

    QPointer<MarketSeriesPickerDialog> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager,
         request]() -> std::expected<md::messaging::get_market_series_response, QString> {
            auto result = clientManager->process_authenticated_request(request);
            if (!result)
                return std::unexpected(QString::fromStdString(result.error()));
            if (!result->success)
                return std::unexpected(QString::fromStdString(result->message));
            return *result;
        });

    using ResultType = std::expected<md::messaging::get_market_series_response, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!result) {
            self->statusLabel_->setText(
                self->tr("Failed to load market series: %1").arg(result.error()));
            self->statusLabel_->setVisible(true);
            return;
        }
        self->statusLabel_->setVisible(false);
        self->rows_ = std::move(result->market_series);
        self->populateTable();
    });
    watcher->setFuture(future);
}

void MarketSeriesPickerDialog::populateTable() {
    const QString filter = filterEdit_->text().trimmed().toLower();
    table_->setRowCount(0);
    for (const auto& s : rows_) {
        if (!filter.isEmpty()) {
            const QString haystack = QString::fromStdString(s.series_type + " " + s.metric + " " +
                                                             s.qualifier)
                                          .toLower();
            if (!haystack.contains(filter))
                continue;
        }
        const int row = table_->rowCount();
        table_->insertRow(row);
        table_->setItem(row, 0, new QTableWidgetItem(QString::fromStdString(s.series_type)));
        table_->setItem(row, 1, new QTableWidgetItem(QString::fromStdString(s.metric)));
        table_->setItem(row, 2, new QTableWidgetItem(QString::fromStdString(s.qualifier)));
        table_->setItem(row,
                        3,
                        new QTableWidgetItem(
                            QString::fromStdString(rfl::enum_to_string(s.series_subclass))));
        table_->item(row, 0)->setData(Qt::UserRole, QString::fromStdString(boost::uuids::to_string(s.id)));
    }
}

void MarketSeriesPickerDialog::onRowActivated(int row) {
    if (row < 0 || row >= table_->rowCount())
        return;
    const QString id = table_->item(row, 0)->data(Qt::UserRole).toString();
    const auto it = std::find_if(rows_.begin(), rows_.end(), [&](const auto& s) {
        return boost::uuids::to_string(s.id) == id.toStdString();
    });
    if (it != rows_.end()) {
        selected_ = *it;
        accept();
    }
}

void MarketSeriesPickerDialog::onSelectClicked() {
    const auto selection = table_->selectionModel()->selectedRows();
    if (selection.isEmpty()) {
        statusLabel_->setText(tr("Select a row first, or use New Series below."));
        statusLabel_->setVisible(true);
        return;
    }
    onRowActivated(selection.first().row());
}

void MarketSeriesPickerDialog::onCreateClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        statusLabel_->setText(tr("Not connected to server."));
        statusLabel_->setVisible(true);
        return;
    }
    const auto seriesType = newSeriesTypeEdit_->text().trimmed();
    const auto metric = newMetricEdit_->text().trimmed();
    if (seriesType.isEmpty() || metric.isEmpty()) {
        statusLabel_->setText(tr("Series Type and Metric are required to create a new series."));
        statusLabel_->setVisible(true);
        return;
    }

    marketdata::domain::market_series series;
    series.series_type = seriesType.toStdString();
    series.metric = metric.toStdString();
    series.qualifier = newQualifierEdit_->text().trimmed().toStdString();
    series.asset_class =
        rfl::string_to_enum<md::domain::asset_class>(newAssetClassCombo_->currentText().toStdString())
            .value();
    series.series_subclass =
        rfl::string_to_enum<md::domain::series_subclass>(
            newSubclassCombo_->currentText().toStdString())
            .value();
    series.is_scalar = false;
    series.derivation_kind = "OBSERVED";

    const auto request = md::messaging::save_market_series_request::from(series);
    QPointer<MarketSeriesPickerDialog> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future =
        QtConcurrent::run([clientManager, request, series]()
                              -> std::expected<marketdata::domain::market_series, QString> {
            auto result = clientManager->process_authenticated_request(request);
            if (!result)
                return std::unexpected(QString::fromStdString(result.error()));
            if (!result->success)
                return std::unexpected(QString::fromStdString(result->message));
            return series;
        });

    using ResultType = std::expected<marketdata::domain::market_series, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!result) {
            self->statusLabel_->setText(self->tr("Create failed: %1").arg(result.error()));
            self->statusLabel_->setVisible(true);
            return;
        }
        // The freshly-created series has no server-assigned id in our local copy -- re-fetch the
        // full list and match on the fields we just sent to find the row the server created.
        const auto created = *result;
        md::messaging::get_market_series_request refetch;
        refetch.offset = 0;
        refetch.limit = 1000;
        QPointer<ClientManager> clientManager2 = self->clientManager_;
        auto future2 = QtConcurrent::run(
            [clientManager2, refetch]()
                -> std::expected<md::messaging::get_market_series_response, QString> {
                auto r = clientManager2->process_authenticated_request(refetch);
                if (!r)
                    return std::unexpected(QString::fromStdString(r.error()));
                if (!r->success)
                    return std::unexpected(QString::fromStdString(r->message));
                return *r;
            });
        using RefetchResult = std::expected<md::messaging::get_market_series_response, QString>;
        auto* watcher2 = new QFutureWatcher<RefetchResult>(self);
        connect(watcher2, &QFutureWatcher<RefetchResult>::finished, self, [self, watcher2, created]() {
            auto r = watcher2->result();
            watcher2->deleteLater();
            if (!self)
                return;
            if (!r) {
                self->statusLabel_->setText(self->tr("Created, but reload failed: %1").arg(r.error()));
                self->statusLabel_->setVisible(true);
                return;
            }
            self->rows_ = std::move(r->market_series);
            self->populateTable();
            const auto it =
                std::find_if(self->rows_.begin(), self->rows_.end(), [&](const auto& s) {
                    return s.series_type == created.series_type && s.metric == created.metric &&
                           s.qualifier == created.qualifier;
                });
            if (it != self->rows_.end()) {
                self->selected_ = *it;
                self->accept();
            }
        });
        watcher2->setFuture(future2);
    });
    watcher->setFuture(future);
}

}
