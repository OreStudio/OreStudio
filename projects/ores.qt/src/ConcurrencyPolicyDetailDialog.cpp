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
#include "ores.qt/ConcurrencyPolicyDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include "ui_ConcurrencyPolicyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ConcurrencyPolicyDetailDialog::ConcurrencyPolicyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ConcurrencyPolicyDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ConcurrencyPolicyDetailDialog::~ConcurrencyPolicyDetailDialog() {
    delete ui_;
}

QTabWidget* ConcurrencyPolicyDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ConcurrencyPolicyDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ConcurrencyPolicyDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ConcurrencyPolicyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ConcurrencyPolicyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ConcurrencyPolicyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ConcurrencyPolicyDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ConcurrencyPolicyDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &ConcurrencyPolicyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &ConcurrencyPolicyDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ConcurrencyPolicyDetailDialog::onFieldChanged);
}

void ConcurrencyPolicyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ConcurrencyPolicyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ConcurrencyPolicyDetailDialog::setPolicy(
    const reporting::domain::concurrency_policy& policy) {
    policy_ = policy;
    updateUiFromPolicy();
}

void ConcurrencyPolicyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ConcurrencyPolicyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ConcurrencyPolicyDetailDialog::updateUiFromPolicy() {
    ui_->codeEdit->setText(QString::fromStdString(policy_.code));
    ui_->nameEdit->setText(QString::fromStdString(policy_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(policy_.description));

    populateProvenance(policy_.version,
                       policy_.modified_by,
                       policy_.performed_by,
                       policy_.recorded_at,
                       policy_.change_reason_code,
                       policy_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ConcurrencyPolicyDetailDialog::updatePolicyFromUi() {
    if (createMode_) {
        policy_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    policy_.name = ui_->nameEdit->text().trimmed().toStdString();
    policy_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    policy_.modified_by = username_;
    policy_.performed_by = username_;
}

void ConcurrencyPolicyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ConcurrencyPolicyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ConcurrencyPolicyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ConcurrencyPolicyDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void ConcurrencyPolicyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save concurrency policy while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updatePolicyFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    policy_.change_reason_code = crSel->reason_code;
    policy_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Saving concurrency policy: " << policy_.code;

    QPointer<ConcurrencyPolicyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, policy = policy_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::save_concurrency_policy_request request;
        request.policy = policy;
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
            BOOST_LOG_SEV(lg(), info) << "Concurrency Policy saved successfully";
            QString code = QString::fromStdString(self->policy_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->policySaved(code);
            self->notifySaveSuccess(tr("Concurrency Policy '%1' saved").arg(code));
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

void ConcurrencyPolicyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete concurrency policy while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(policy_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Concurrency Policy",
        QString("Are you sure you want to delete concurrency policy '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting concurrency policy: " << policy_.code;

    QPointer<ConcurrencyPolicyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = policy_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        reporting::messaging::delete_concurrency_policy_request request;
        request.codes = {code};
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
            BOOST_LOG_SEV(lg(), info) << "Concurrency Policy deleted successfully";
            emit self->statusMessage(QString("Concurrency Policy '%1' deleted").arg(code));
            emit self->policyDeleted(code);
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
