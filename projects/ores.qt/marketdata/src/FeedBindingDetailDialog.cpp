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
#include "ores.qt/FeedBindingDetailDialog.hpp"
#include ""
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_FeedBindingDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

FeedBindingDetailDialog::FeedBindingDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::FeedBindingDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

FeedBindingDetailDialog::~FeedBindingDetailDialog() {
    delete ui_;
}

QTabWidget* FeedBindingDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FeedBindingDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FeedBindingDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FeedBindingDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FeedBindingDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onCloseClicked);
}

void FeedBindingDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FeedBindingDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FeedBindingDetailDialog::setBinding(const&) {
    _ = ;
    updateUiFromBinding();
}

void FeedBindingDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void FeedBindingDetailDialog::updateUiFromBinding() {

    populateProvenance(_.version,
                       _.modified_by,
                       _.performed_by,
                       _.recorded_at,
                       _.change_reason_code,
                       _.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::updateBindingFromUi() {
    _.modified_by = username_;
}

void FeedBindingDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool FeedBindingDetailDialog::validateInput() {

    return true;
}

void FeedBindingDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save feed binding while disconnected from server.");
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
    _.change_reason_code = crSel->reason_code;
    _.change_commentary = crSel->commentary;

    updateBindingFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving feed binding: " << _.;

    QPointer<FeedBindingDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, = _]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        request;
        request.data = ;
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
            BOOST_LOG_SEV(lg(), info) << "Feed Binding saved successfully";
            QString code = QString::fromStdString(self->_.);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->Saved(code);
            self->notifySaveSuccess(tr("Feed Binding '%1' saved").arg(code));
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

void FeedBindingDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete feed binding while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(_.);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Feed Binding",
        QString("Are you sure you want to delete feed binding '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting feed binding: " << _.;

    QPointer<FeedBindingDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = _.]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        request;
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
            BOOST_LOG_SEV(lg(), info) << "Feed Binding deleted successfully";
            emit self->statusMessage(QString("Feed Binding '%1' deleted").arg(code));
            emit self->Deleted(code);
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
