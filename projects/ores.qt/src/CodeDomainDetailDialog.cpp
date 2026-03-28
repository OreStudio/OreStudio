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
#include "ores.qt/CodeDomainDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include "ui_CodeDomainDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CodeDomainDetailDialog::CodeDomainDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CodeDomainDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CodeDomainDetailDialog::~CodeDomainDetailDialog() {
    delete ui_;
}

QTabWidget* CodeDomainDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CodeDomainDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CodeDomainDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CodeDomainDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CodeDomainDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CodeDomainDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CodeDomainDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CodeDomainDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &CodeDomainDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &CodeDomainDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &CodeDomainDetailDialog::onFieldChanged);
}

void CodeDomainDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CodeDomainDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CodeDomainDetailDialog::setDomain(
    const dq::domain::code_domain& domain) {
    domain_ = domain;
    updateUiFromDomain();
}

void CodeDomainDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CodeDomainDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CodeDomainDetailDialog::updateUiFromDomain() {
    ui_->codeEdit->setText(QString::fromStdString(domain_.code));
    ui_->nameEdit->setText(QString::fromStdString(domain_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(domain_.description));

    populateProvenance(domain_.version,
                       domain_.modified_by,
                       domain_.performed_by,
                       domain_.recorded_at,
                       domain_.change_reason_code,
                       domain_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CodeDomainDetailDialog::updateDomainFromUi() {
    if (createMode_) {
        domain_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    domain_.name = ui_->nameEdit->text().trimmed().toStdString();
    domain_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    domain_.modified_by = username_;
    domain_.performed_by = username_;
}

void CodeDomainDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CodeDomainDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CodeDomainDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CodeDomainDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void CodeDomainDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save code domain while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateDomainFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving code domain: " << domain_.code;

    QPointer<CodeDomainDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, domain = domain_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_code_domain_request request;
        request.data = domain;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, response_result.error()};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Code Domain saved successfully";
            QString code = QString::fromStdString(self->domain_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->domainSaved(code);
            self->notifySaveSuccess(tr("Code Domain '%1' saved").arg(code));
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

void CodeDomainDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete code domain while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(domain_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Code Domain",
        QString("Are you sure you want to delete code domain '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting code domain: " << domain_.code;

    QPointer<CodeDomainDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code_str = domain_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_code_domain_request request;
        request.codes = {code_str};
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, response_result.error()};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Code Domain deleted successfully";
            emit self->statusMessage(QString("Code Domain '%1' deleted").arg(code));
            emit self->domainDeleted(code);
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
