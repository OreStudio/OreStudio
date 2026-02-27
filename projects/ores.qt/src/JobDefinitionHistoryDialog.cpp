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
#include "ores.qt/JobDefinitionHistoryDialog.hpp"

#include <QHeaderView>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_JobDefinitionHistoryDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

QString format_status(scheduler::domain::job_status s) {
    switch (s) {
    case scheduler::domain::job_status::starting:   return QObject::tr("Starting");
    case scheduler::domain::job_status::succeeded:  return QObject::tr("Succeeded");
    case scheduler::domain::job_status::failed:     return QObject::tr("Failed");
    }
    return QObject::tr("Unknown");
}

QString format_duration(const scheduler::domain::job_instance& instance) {
    if (auto dur = instance.duration()) {
        const auto secs = dur->count();
        if (secs < 60)
            return QString("%1s").arg(secs);
        return QString("%1m %2s").arg(secs / 60).arg(secs % 60);
    }
    return QObject::tr("Running…");
}

} // anonymous namespace

JobDefinitionHistoryDialog::JobDefinitionHistoryDialog(
    const boost::uuids::uuid& job_definition_id,
    const QString& job_name,
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::JobDefinitionHistoryDialog),
      job_definition_id_(job_definition_id),
      job_name_(job_name),
      clientManager_(clientManager) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

JobDefinitionHistoryDialog::~JobDefinitionHistoryDialog() {
    delete ui_;
}

void JobDefinitionHistoryDialog::setupUi() {
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    ui_->titleLabel->setText(QString("Execution history: %1").arg(job_name_));

    // Columns: Run ID | Status | Start Time | End Time | Duration | Message
    ui_->versionListWidget->setColumnCount(6);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {tr("Run ID"), tr("Status"), tr("Start Time"),
         tr("End Time"), tr("Duration"), tr("Message")});
    ui_->versionListWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->versionListWidget->setEditTriggers(QAbstractItemView::NoEditTriggers);
}

void JobDefinitionHistoryDialog::setupConnections() {
    connect(ui_->versionListWidget, &QTableWidget::itemSelectionChanged,
            this, &JobDefinitionHistoryDialog::onRunSelected);
    connect(ui_->closeButton, &QPushButton::clicked,
            this, [this]() { if (window()) window()->close(); });
}

void JobDefinitionHistoryDialog::loadHistory() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading execution history for: "
                               << job_name_.toStdString();
    emit statusChanged(tr("Loading execution history…"));

    QPointer<JobDefinitionHistoryDialog> self = this;

    struct HistoryResult {
        bool success;
        std::string message;
        std::vector<scheduler::domain::job_instance> instances;
    };

    auto task = [self, id = job_definition_id_]() -> HistoryResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed", {}};
        }

        scheduler::messaging::get_job_history_request request;
        request.job_definition_id = id;
        request.limit = 0; // server default (100)
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_job_history_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server", {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response", {}};
        }

        auto response = scheduler::messaging::get_job_history_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response", {}};
        }

        return {response->success, response->message,
                std::move(response->instances)};
    };

    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            self->instances_ = std::move(result.instances);
            self->updateRunList();
            emit self->statusChanged(
                QString("Loaded %1 execution record(s)").arg(
                    self->instances_.size()));
        } else {
            BOOST_LOG_SEV(lg(), error) << "History load failed: "
                                       << result.message;
            emit self->errorOccurred(QString::fromStdString(result.message));
        }
    });

    QFuture<HistoryResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void JobDefinitionHistoryDialog::updateRunList() {
    ui_->versionListWidget->setRowCount(0);

    for (const auto& inst : instances_) {
        int row = ui_->versionListWidget->rowCount();
        ui_->versionListWidget->insertRow(row);

        auto* runIdItem = new QTableWidgetItem(
            QString::number(inst.instance_id));
        runIdItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        ui_->versionListWidget->setItem(row, 0, runIdItem);

        auto* statusItem = new QTableWidgetItem(format_status(inst.status));
        ui_->versionListWidget->setItem(row, 1, statusItem);

        auto* startItem = new QTableWidgetItem(
            relative_time_helper::format(inst.start_time));
        ui_->versionListWidget->setItem(row, 2, startItem);

        QString endText = inst.end_time
            ? relative_time_helper::format(*inst.end_time)
            : tr("—");
        auto* endItem = new QTableWidgetItem(endText);
        ui_->versionListWidget->setItem(row, 3, endItem);

        auto* durationItem = new QTableWidgetItem(format_duration(inst));
        ui_->versionListWidget->setItem(row, 4, durationItem);

        auto* msgItem = new QTableWidgetItem(
            QString::fromStdString(inst.return_message));
        ui_->versionListWidget->setItem(row, 5, msgItem);
    }

    if (!instances_.empty()) {
        ui_->versionListWidget->selectRow(0);
    }
}

void JobDefinitionHistoryDialog::onRunSelected() {
    // No-op: the table is read-only; selection just highlights the row.
}

}
