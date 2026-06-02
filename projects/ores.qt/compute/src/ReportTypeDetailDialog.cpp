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
#include "ores.qt/ReportTypeDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include "ui_ReportTypeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.reporting.api/messaging/report_type_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportTypeDetailDialog::ReportTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ReportTypeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ReportTypeDetailDialog::~ReportTypeDetailDialog() {
    delete ui_;
}

QTabWidget* ReportTypeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ReportTypeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ReportTypeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ReportTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ReportTypeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ReportTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ReportTypeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ReportTypeDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &ReportTypeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ReportTypeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ReportTypeDetailDialog::onFieldChanged);
}

void ReportTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ReportTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ReportTypeDetailDialog::setType(
    const reporting::domain::report_type& type) {
    type_ = type;
    updateUiFromType();
}

void ReportTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ReportTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(type_.code));
    ui_->nameEdit->setText(QString::fromStdString(type_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(type_.description));

    populateProvenance(type_.version,
                       type_.modified_by,
                       type_.performed_by,
                       type_.recorded_at,
                       type_.change_reason_code,
                       type_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ReportTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    type_.name = ui_->nameEdit->text().trimmed().toStdString();
    type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    type_.modified_by = username_;
    type_.performed_by = username_;
}

void ReportTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ReportTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ReportTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void ReportTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save report type while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTypeFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    type_.change_reason_code = crSel->reason_code;
    type_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Saving report type: " << type_.code;

    QPointer<ReportTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, type = type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::save_report_type_request request;
        request.type = type;
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }


        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Type saved successfully";
            QString code = QString::fromStdString(self->type_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->typeSaved(code);
            self->notifySaveSuccess(tr("Report Type '%1' saved").arg(code));
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

void ReportTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete report type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(type_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Report Type",
        QString("Are you sure you want to delete report type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting report type: " << type_.code;

    QPointer<ReportTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = type_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::delete_report_type_request request;
        request.codes.push_back(code);
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Report Type deleted successfully";
            emit self->statusMessage(QString("Report Type '%1' deleted").arg(code));
            emit self->typeDeleted(code);
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
