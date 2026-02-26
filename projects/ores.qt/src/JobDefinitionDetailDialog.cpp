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
#include "ores.qt/JobDefinitionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <chrono>
#include <boost/uuid/random_generator.hpp>
#include "ui_JobDefinitionDetailDialog.h"
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"
#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = dq::domain::change_reason_constants;

JobDefinitionDetailDialog::JobDefinitionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::JobDefinitionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

JobDefinitionDetailDialog::~JobDefinitionDetailDialog() {
    delete ui_;
}

QTabWidget* JobDefinitionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* JobDefinitionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* JobDefinitionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void JobDefinitionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarCancel, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void JobDefinitionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &JobDefinitionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &JobDefinitionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &JobDefinitionDetailDialog::onCloseClicked);

    connect(ui_->jobNameEdit, &QLineEdit::textChanged, this,
            &JobDefinitionDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &JobDefinitionDetailDialog::onFieldChanged);
    connect(ui_->commandEdit, &QPlainTextEdit::textChanged, this,
            &JobDefinitionDetailDialog::onFieldChanged);
    connect(ui_->scheduleExpressionEdit, &QLineEdit::textChanged, this,
            &JobDefinitionDetailDialog::onFieldChanged);
    connect(ui_->databaseNameEdit, &QLineEdit::textChanged, this,
            &JobDefinitionDetailDialog::onFieldChanged);
}

void JobDefinitionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void JobDefinitionDetailDialog::setChangeReasonCache(ChangeReasonCache* cache) {
    changeReasonCache_ = cache;
}

void JobDefinitionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void JobDefinitionDetailDialog::setDefinition(
    const scheduler::domain::job_definition& definition) {
    definition_ = definition;
    updateUiFromDefinition();
}

void JobDefinitionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->jobNameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        definition_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void JobDefinitionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->jobNameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->commandEdit->setReadOnly(readOnly);
    ui_->scheduleExpressionEdit->setReadOnly(readOnly);
    ui_->databaseNameEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void JobDefinitionDetailDialog::updateUiFromDefinition() {
    ui_->jobNameEdit->setText(QString::fromStdString(definition_.job_name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(definition_.description));
    ui_->commandEdit->setPlainText(QString::fromStdString(definition_.command));
    ui_->scheduleExpressionEdit->setText(
        QString::fromStdString(definition_.schedule_expression.to_string()));
    ui_->databaseNameEdit->setText(QString::fromStdString(definition_.database_name));

    populateProvenance(definition_.version,
                       definition_.modified_by,
                       /*performed_by=*/"",
                       /*recorded_at=*/std::chrono::system_clock::time_point{},
                       /*change_reason_code=*/"",
                       /*change_commentary=*/"");

    hasChanges_ = false;
    updateSaveButtonState();
}

void JobDefinitionDetailDialog::updateDefinitionFromUi() {
    if (createMode_) {
        definition_.job_name = ui_->jobNameEdit->text().trimmed().toStdString();
    }
    definition_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    definition_.command = ui_->commandEdit->toPlainText().trimmed().toStdString();
    const auto cron_str = ui_->scheduleExpressionEdit->text().trimmed().toStdString();
    if (auto result = scheduler::domain::cron_expression::from_string(cron_str)) {
        definition_.schedule_expression = *result;
    }
    definition_.database_name = ui_->databaseNameEdit->text().trimmed().toStdString();
    definition_.modified_by = username_;
}

void JobDefinitionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void JobDefinitionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void JobDefinitionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool JobDefinitionDetailDialog::validateInput() {
    const QString job_name_val = ui_->jobNameEdit->text().trimmed();
    const QString command_val = ui_->commandEdit->toPlainText().trimmed();
    const QString schedule_expression_val = ui_->scheduleExpressionEdit->text().trimmed();
    const QString database_name_val = ui_->databaseNameEdit->text().trimmed();

    return !job_name_val.isEmpty() && !command_val.isEmpty() && !schedule_expression_val.isEmpty() && !database_name_val.isEmpty();
}

void JobDefinitionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save job definition while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    // Change reason dialog (amend mode only)
    std::string change_reason_code;
    std::string change_commentary;
    if (!createMode_) {
        if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
            emit errorMessage(tr("Change reasons not loaded. Please try again."));
            return;
        }
        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (reasons.empty()) {
            emit errorMessage(tr("No change reasons available. Please contact administrator."));
            return;
        }
        ChangeReasonDialog dlg(reasons, ChangeReasonDialog::OperationType::Amend,
                               hasChanges_, this);
        if (dlg.exec() != QDialog::Accepted)
            return;
        change_reason_code = dlg.selectedReasonCode();
        change_commentary  = dlg.commentary();
    }

    updateDefinitionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving job definition: " << definition_.job_name;

    QPointer<JobDefinitionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, definition = definition_,
                 change_reason_code, change_commentary]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        scheduler::messaging::schedule_job_request request;
        request.definition = definition;
        request.change_reason_code = change_reason_code;
        request.change_commentary  = change_commentary;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::schedule_job_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = scheduler::messaging::schedule_job_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Job Definition saved successfully";
            QString code = QString::fromStdString(self->definition_.job_name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->definitionSaved(code);
            self->notifySaveSuccess(tr("Job Definition '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void JobDefinitionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete job definition while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(definition_.job_name);
    auto reply = MessageBoxHelper::question(this, "Unschedule Job Definition",
        QString("Are you sure you want to unschedule job definition '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    // Change reason dialog for delete
    std::string change_reason_code;
    std::string change_commentary;
    if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
        emit errorMessage(tr("Change reasons not loaded. Please try again."));
        return;
    }
    auto reasons = changeReasonCache_->getReasonsForDelete(
        std::string{reason::categories::common});
    if (reasons.empty()) {
        emit errorMessage(tr("No change reasons available. Please contact administrator."));
        return;
    }
    ChangeReasonDialog dlg(reasons, ChangeReasonDialog::OperationType::Delete,
                           false, this);
    if (dlg.exec() != QDialog::Accepted)
        return;
    change_reason_code = dlg.selectedReasonCode();
    change_commentary  = dlg.commentary();

    BOOST_LOG_SEV(lg(), info) << "Unscheduling job definition: " << definition_.job_name;

    QPointer<JobDefinitionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = definition_.id,
                 change_reason_code, change_commentary]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        scheduler::messaging::unschedule_job_request request;
        request.job_definition_id = id;
        request.change_reason_code = change_reason_code;
        request.change_commentary  = change_commentary;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::unschedule_job_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = scheduler::messaging::unschedule_job_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Job Definition unscheduled successfully";
            emit self->statusMessage(QString("Job Definition '%1' unscheduled").arg(code));
            emit self->definitionDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
