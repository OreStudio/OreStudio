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
#include "ores.qt/ReportDefinitionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_ReportDefinitionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportDefinitionDetailDialog::ReportDefinitionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ReportDefinitionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ReportDefinitionDetailDialog::~ReportDefinitionDetailDialog() {
    delete ui_;
}

QTabWidget* ReportDefinitionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ReportDefinitionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ReportDefinitionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ReportDefinitionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ReportDefinitionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ReportDefinitionDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->reportTypeEdit, &QLineEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->scheduleExpressionEdit, &QLineEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
    connect(ui_->concurrencyPolicyEdit, &QLineEdit::textChanged, this,
            &ReportDefinitionDetailDialog::onFieldChanged);
}

void ReportDefinitionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ReportDefinitionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ReportDefinitionDetailDialog::setDefinition(
    const reporting::domain::report_definition& definition) {
    definition_ = definition;
    updateUiFromDefinition();
}

void ReportDefinitionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        definition_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->reportTypeEdit->setReadOnly(readOnly);
    ui_->scheduleExpressionEdit->setReadOnly(readOnly);
    ui_->concurrencyPolicyEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ReportDefinitionDetailDialog::updateUiFromDefinition() {
    ui_->nameEdit->setText(QString::fromStdString(definition_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(definition_.description));
    ui_->reportTypeEdit->setText(QString::fromStdString(definition_.report_type));
    ui_->scheduleExpressionEdit->setText(QString::fromStdString(definition_.schedule_expression));
    ui_->concurrencyPolicyEdit->setText(QString::fromStdString(definition_.concurrency_policy));

    populateProvenance(definition_.version,
                       definition_.modified_by,
                       definition_.performed_by,
                       definition_.recorded_at,
                       definition_.change_reason_code,
                       definition_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::updateDefinitionFromUi() {
    if (createMode_) {
        definition_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    definition_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    definition_.report_type = ui_->reportTypeEdit->text().trimmed().toStdString();
    definition_.schedule_expression = ui_->scheduleExpressionEdit->text().trimmed().toStdString();
    definition_.concurrency_policy = ui_->concurrencyPolicyEdit->text().trimmed().toStdString();
    definition_.modified_by = username_;
    definition_.performed_by = username_;
}

void ReportDefinitionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportDefinitionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ReportDefinitionDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString report_type_val = ui_->reportTypeEdit->text().trimmed();
    const QString schedule_expression_val = ui_->scheduleExpressionEdit->text().trimmed();
    const QString concurrency_policy_val = ui_->concurrencyPolicyEdit->text().trimmed();

    return !name_val.isEmpty() && !report_type_val.isEmpty() && !schedule_expression_val.isEmpty() && !concurrency_policy_val.isEmpty();
}

void ReportDefinitionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save report definition while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateDefinitionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving report definition: " << definition_.name;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, definition = definition_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::save_report_definition_request request;
        request.definition = definition;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_report_definition_request,
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

        auto response = reporting::messaging::save_report_definition_response::
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
            BOOST_LOG_SEV(lg(), info) << "Report Definition saved successfully";
            QString code = QString::fromStdString(self->definition_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->definitionSaved(code);
            self->notifySaveSuccess(tr("Report Definition '%1' saved").arg(code));
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

void ReportDefinitionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete report definition while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(definition_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Report Definition",
        QString("Are you sure you want to delete report definition '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting report definition: " << definition_.name;

    QPointer<ReportDefinitionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = definition_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::delete_report_definition_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_report_definition_request,
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

        auto response = reporting::messaging::delete_report_definition_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Definition deleted successfully";
            emit self->statusMessage(QString("Report Definition '%1' deleted").arg(code));
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
