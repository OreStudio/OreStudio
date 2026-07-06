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
#include "ores.qt/CurrencyPairClassificationDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairClassificationDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CurrencyPairClassificationDetailDialog::CurrencyPairClassificationDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyPairClassificationDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CurrencyPairClassificationDetailDialog::~CurrencyPairClassificationDetailDialog() {
    delete ui_;
}

QTabWidget* CurrencyPairClassificationDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CurrencyPairClassificationDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CurrencyPairClassificationDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CurrencyPairClassificationDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CurrencyPairClassificationDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairClassificationDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairClassificationDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairClassificationDetailDialog::onCloseClicked);

    connect(ui_->codeEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairClassificationDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairClassificationDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &CurrencyPairClassificationDetailDialog::onFieldChanged);
}

void CurrencyPairClassificationDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CurrencyPairClassificationDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyPairClassificationDetailDialog::setClassification(
    const refdata::domain::currency_pair_classification& classification) {
    classification_ = classification;
    updateUiFromClassification();
}

void CurrencyPairClassificationDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairClassificationDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairClassificationDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CurrencyPairClassificationDetailDialog::updateUiFromClassification() {
    ui_->codeEdit->setText(QString::fromStdString(classification_.code));
    ui_->nameEdit->setText(QString::fromStdString(classification_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(classification_.description));

    populateProvenance(classification_.version,
                       classification_.modified_by,
                       classification_.performed_by,
                       classification_.recorded_at,
                       classification_.change_reason_code,
                       classification_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairClassificationDetailDialog::updateClassificationFromUi() {
    if (createMode_) {
        classification_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    classification_.name = ui_->nameEdit->text().trimmed().toStdString();
    classification_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    classification_.modified_by = username_;
}

void CurrencyPairClassificationDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairClassificationDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairClassificationDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CurrencyPairClassificationDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void CurrencyPairClassificationDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save currency pair classification while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    classification_.change_reason_code = crSel->reason_code;
    classification_.change_commentary = crSel->commentary;

    updateClassificationFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving currency pair classification: " << classification_.code;

    QPointer<CurrencyPairClassificationDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, classification = classification_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_currency_pair_classification_request request;
        request.data = classification;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Classification saved successfully";
            QString code = QString::fromStdString(self->classification_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->classificationSaved(code);
            self->notifySaveSuccess(tr("Currency Pair Classification '%1' saved").arg(code));
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

void CurrencyPairClassificationDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete currency pair classification while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(classification_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Currency Pair Classification",
        QString("Are you sure you want to delete currency pair classification '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting currency pair classification: " << classification_.code;

    QPointer<CurrencyPairClassificationDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = classification_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_currency_pair_classification_request request;
        request.codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Classification deleted successfully";
            emit self->statusMessage(
                QString("Currency Pair Classification '%1' deleted").arg(code));
            emit self->classificationDeleted(code);
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
