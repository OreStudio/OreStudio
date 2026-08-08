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
#include "ores.qt/BootstrapConfigPickerDialog.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
#include <QAbstractItemView>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {
namespace rd = ores::refdata;
}

BootstrapConfigPickerDialog::BootstrapConfigPickerDialog(ClientManager* clientManager,
                                                          QWidget* parent)
    : QDialog(parent)
    , clientManager_(clientManager) {
    setWindowTitle(tr("Select Discount Curve Config"));
    resize(640, 420);

    auto* layout = new QVBoxLayout(this);
    layout->addWidget(new QLabel(
        tr("Existing FUNDING-role bootstrap configs (the discount curve to project from):"),
        this));

    table_ = new QTableWidget(0, 4, this);
    table_->setHorizontalHeaderLabels(
        {tr("Output Series Id"), tr("Interpolation"), tr("Day Count"), tr("Version")});
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
    connect(selectButton_,
            &QPushButton::clicked,
            this,
            &BootstrapConfigPickerDialog::onSelectClicked);
    auto* cancelButton = new QPushButton(tr("Cancel"), this);
    connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
    buttonsRow->addStretch();
    buttonsRow->addWidget(selectButton_);
    buttonsRow->addWidget(cancelButton);
    layout->addLayout(buttonsRow);

    statusLabel_ = new QLabel(this);
    statusLabel_->setStyleSheet("color: #f87171;");
    statusLabel_->setWordWrap(true);
    statusLabel_->setVisible(false);
    layout->addWidget(statusLabel_);

    reload();
}

void BootstrapConfigPickerDialog::reload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        statusLabel_->setText(tr("Not connected to server."));
        statusLabel_->setVisible(true);
        return;
    }

    rd::messaging::get_ir_curve_bootstrap_configs_request request;
    request.offset = 0;
    request.limit = 1000;

    QPointer<BootstrapConfigPickerDialog> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager,
         request]() -> std::expected<rd::messaging::get_ir_curve_bootstrap_configs_response, QString> {
            auto result = clientManager->process_authenticated_request(request);
            if (!result)
                return std::unexpected(QString::fromStdString(result.error()));
            if (!result->success)
                return std::unexpected(QString::fromStdString(result->message));
            return *result;
        });

    using ResultType =
        std::expected<rd::messaging::get_ir_curve_bootstrap_configs_response, QString>;
    auto* watcher = new QFutureWatcher<ResultType>(this);
    connect(watcher, &QFutureWatcher<ResultType>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!result) {
            self->statusLabel_->setText(
                self->tr("Failed to load bootstrap configs: %1").arg(result.error()));
            self->statusLabel_->setVisible(true);
            return;
        }
        self->statusLabel_->setVisible(false);
        self->rows_.clear();
        for (auto& c : result->bootstrap_configs) {
            if (c.curve_family_role == "FUNDING")
                self->rows_.push_back(std::move(c));
        }

        self->table_->setRowCount(0);
        for (const auto& c : self->rows_) {
            const int row = self->table_->rowCount();
            self->table_->insertRow(row);
            self->table_->setItem(
                row,
                0,
                new QTableWidgetItem(QString::fromStdString(boost::uuids::to_string(c.output_series_id))));
            self->table_->setItem(
                row, 1, new QTableWidgetItem(QString::fromStdString(c.interpolation_method)));
            self->table_->setItem(
                row, 2, new QTableWidgetItem(QString::fromStdString(c.day_count_convention)));
            self->table_->setItem(row, 3, new QTableWidgetItem(QString::number(c.version)));
            self->table_->item(row, 0)->setData(
                Qt::UserRole, QString::fromStdString(boost::uuids::to_string(c.id)));
        }
    });
    watcher->setFuture(future);
}

void BootstrapConfigPickerDialog::onRowActivated(int row) {
    if (row < 0 || row >= table_->rowCount())
        return;
    const QString id = table_->item(row, 0)->data(Qt::UserRole).toString();
    const auto it = std::find_if(rows_.begin(), rows_.end(), [&](const auto& c) {
        return boost::uuids::to_string(c.id) == id.toStdString();
    });
    if (it != rows_.end()) {
        selected_ = *it;
        accept();
    }
}

void BootstrapConfigPickerDialog::onSelectClicked() {
    const auto selection = table_->selectionModel()->selectedRows();
    if (selection.isEmpty()) {
        statusLabel_->setText(tr("Select a row first."));
        statusLabel_->setVisible(true);
        return;
    }
    onRowActivated(selection.first().row());
}

}
