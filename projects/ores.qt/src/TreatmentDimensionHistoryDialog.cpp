/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/TreatmentDimensionHistoryDialog.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_TreatmentDimensionHistoryDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TreatmentDimensionHistoryDialog::TreatmentDimensionHistoryDialog(
    const QString& code, ClientManager* clientManager, QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::TreatmentDimensionHistoryDialog),
      code_(code),
      clientManager_(clientManager),
      toolbar_(nullptr),
      openVersionAction_(nullptr),
      revertAction_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupToolbar();
    setupConnections();
}

TreatmentDimensionHistoryDialog::~TreatmentDimensionHistoryDialog() {
    delete ui_;
}

void TreatmentDimensionHistoryDialog::setupUi() {
    ui_->titleLabel->setText(QString("History for: %1").arg(code_));
    ui_->versionListWidget->setColumnCount(4);
    ui_->versionListWidget->setHorizontalHeaderLabels({"Version", "Recorded At", "Recorded By", "Commentary"});
    ui_->versionListWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->changesTableWidget->setColumnCount(3);
    ui_->changesTableWidget->setHorizontalHeaderLabels({"Field", "Old Value", "New Value"});
    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
}

void TreatmentDimensionHistoryDialog::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    openVersionAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Open, IconUtils::DefaultIconColor), tr("Open"));
    openVersionAction_->setToolTip(tr("Open this version (read-only)"));
    openVersionAction_->setEnabled(false);

    revertAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor),
        tr("Revert"));
    revertAction_->setToolTip(tr("Revert to this version"));
    revertAction_->setEnabled(false);

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void TreatmentDimensionHistoryDialog::setupConnections() {
    connect(ui_->versionListWidget, &QTableWidget::itemSelectionChanged, this, &TreatmentDimensionHistoryDialog::onVersionSelected);
    connect(openVersionAction_, &QAction::triggered, this, &TreatmentDimensionHistoryDialog::onOpenVersionClicked);
    connect(revertAction_, &QAction::triggered, this, &TreatmentDimensionHistoryDialog::onRevertClicked);
}

void TreatmentDimensionHistoryDialog::loadHistory() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit statusChanged(tr("Loading history..."));

    QPointer<TreatmentDimensionHistoryDialog> self = this;
    struct HistoryResult { bool success; std::string message; std::vector<dq::domain::treatment_dimension> versions; };

    auto task = [self, code = code_.toStdString()]() -> HistoryResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed", {}};

        dq::messaging::get_treatment_dimension_history_request request;
        request.code = code;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_treatment_dimension_history_request, 0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server", {}};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response", {}};

        auto response = dq::messaging::get_treatment_dimension_history_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response", {}};

        return {response->success, response->message, std::move(response->versions)};
    };

    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            self->versions_ = std::move(result.versions);
            self->updateVersionList();
            emit self->statusChanged(QString("Loaded %1 versions").arg(self->versions_.size()));
        } else {
            emit self->errorOccurred(QString::fromStdString(result.message));
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void TreatmentDimensionHistoryDialog::updateVersionList() {
    ui_->versionListWidget->setRowCount(0);

    for (const auto& version : versions_) {
        int row = ui_->versionListWidget->rowCount();
        ui_->versionListWidget->insertRow(row);

        auto* versionItem = new QTableWidgetItem(QString::number(version.version));
        versionItem->setTextAlignment(Qt::AlignCenter);
        ui_->versionListWidget->setItem(row, 0, versionItem);
        ui_->versionListWidget->setItem(row, 1, new QTableWidgetItem(relative_time_helper::format(version.recorded_at)));
        ui_->versionListWidget->setItem(row, 2, new QTableWidgetItem(QString::fromStdString(version.recorded_by)));
        ui_->versionListWidget->setItem(row, 3, new QTableWidgetItem(QString::fromStdString(version.change_commentary)));
    }

    if (!versions_.empty()) ui_->versionListWidget->selectRow(0);
}

void TreatmentDimensionHistoryDialog::onVersionSelected() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) { updateActionStates(); return; }

    int row = selected.first()->row();
    updateChangesTable(row);
    updateFullDetails(row);
    updateActionStates();
}

void TreatmentDimensionHistoryDialog::updateChangesTable(int currentVersionIndex) {
    ui_->changesTableWidget->setRowCount(0);

    if (currentVersionIndex < 0 || static_cast<size_t>(currentVersionIndex) >= versions_.size()) return;

    int previousVersionIndex = currentVersionIndex + 1;
    if (static_cast<size_t>(previousVersionIndex) >= versions_.size()) {
        ui_->changesTableWidget->insertRow(0);
        ui_->changesTableWidget->setItem(0, 0, new QTableWidgetItem("(Initial version)"));
        ui_->changesTableWidget->setItem(0, 1, new QTableWidgetItem("-"));
        ui_->changesTableWidget->setItem(0, 2, new QTableWidgetItem("-"));
        return;
    }

    const auto& current = versions_[currentVersionIndex];
    const auto& previous = versions_[previousVersionIndex];

    auto addChange = [this](const QString& field, const QString& oldVal, const QString& newVal) {
        int row = ui_->changesTableWidget->rowCount();
        ui_->changesTableWidget->insertRow(row);
        ui_->changesTableWidget->setItem(row, 0, new QTableWidgetItem(field));
        ui_->changesTableWidget->setItem(row, 1, new QTableWidgetItem(oldVal));
        ui_->changesTableWidget->setItem(row, 2, new QTableWidgetItem(newVal));
    };

    if (current.name != previous.name) addChange("Name", QString::fromStdString(previous.name), QString::fromStdString(current.name));
    if (current.description != previous.description) addChange("Description", QString::fromStdString(previous.description), QString::fromStdString(current.description));

    if (ui_->changesTableWidget->rowCount() == 0) {
        ui_->changesTableWidget->insertRow(0);
        ui_->changesTableWidget->setItem(0, 0, new QTableWidgetItem("(No field changes)"));
        ui_->changesTableWidget->setItem(0, 1, new QTableWidgetItem("-"));
        ui_->changesTableWidget->setItem(0, 2, new QTableWidgetItem("-"));
    }
}

void TreatmentDimensionHistoryDialog::updateFullDetails(int versionIndex) {
    if (versionIndex < 0 || static_cast<size_t>(versionIndex) >= versions_.size()) return;

    const auto& version = versions_[versionIndex];
    ui_->codeValue->setText(QString::fromStdString(version.code));
    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->recordedByValue->setText(QString::fromStdString(version.recorded_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void TreatmentDimensionHistoryDialog::updateActionStates() {
    auto selected = ui_->versionListWidget->selectedItems();
    bool hasSelection = !selected.isEmpty();
    bool isNotLatest = hasSelection && selected.first()->row() > 0;

    openVersionAction_->setEnabled(hasSelection);
    revertAction_->setEnabled(isNotLatest);
}

void TreatmentDimensionHistoryDialog::onOpenVersionClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;

    int row = selected.first()->row();
    if (static_cast<size_t>(row) >= versions_.size()) return;

    emit openVersionRequested(versions_[row], versions_[row].version);
}

void TreatmentDimensionHistoryDialog::onRevertClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;

    int row = selected.first()->row();
    if (static_cast<size_t>(row) >= versions_.size()) return;

    emit revertVersionRequested(versions_[row]);
}

}
