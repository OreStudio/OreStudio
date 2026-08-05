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
#include "ores.qt/ChangeReasonDetailDialog.hpp"
#include "ores.dq.api/messaging/change_reason_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_ChangeReasonDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

ChangeReasonDetailDialog::ChangeReasonDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::ChangeReasonDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

ChangeReasonDetailDialog::~ChangeReasonDetailDialog() {
    delete ui_;
}

QTabWidget* ChangeReasonDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ChangeReasonDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ChangeReasonDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString ChangeReasonDetailDialog::code() const {
    return QString::fromStdString(reason_.code);
}

void ChangeReasonDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ChangeReasonDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &ChangeReasonDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &ChangeReasonDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &ChangeReasonDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &ChangeReasonDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->categoryCodeEdit,
            &QLineEdit::textChanged,
            this,
            &ChangeReasonDetailDialog::onFieldChanged);
}

void ChangeReasonDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ChangeReasonDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ChangeReasonDetailDialog::setReason(const dq::domain::change_reason& reason) {
    reason_ = reason;
    updateUiFromReason();
}

void ChangeReasonDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->categoryCodeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ChangeReasonDetailDialog::updateUiFromReason() {
    ui_->codeEdit->setText(QString::fromStdString(reason_.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(reason_.description));
    ui_->categoryCodeEdit->setText(QString::fromStdString(reason_.category_code));

    populateProvenance(reason_.version,
                       reason_.modified_by,
                       reason_.performed_by,
                       reason_.recorded_at,
                       reason_.change_reason_code,
                       reason_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::updateReasonFromUi() {
    if (createMode_) {
        reason_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    reason_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    reason_.category_code = ui_->categoryCodeEdit->text().trimmed().toStdString();
    reason_.modified_by = username_;
}

void ChangeReasonDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ChangeReasonDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();

    return true && !code_val.isEmpty();
}

void ChangeReasonDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save change reason while disconnected from server.");
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
    reason_.change_reason_code = crSel->reason_code;
    reason_.change_commentary = crSel->commentary;

    updateReasonFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving change reason: " << reason_.code;

    QPointer<ChangeReasonDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, reason = reason_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_change_reason_request request;
        request.data = reason;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Change Reason saved successfully";
                    QString code = QString::fromStdString(self->reason_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->reasonSaved(code);
                    self->notifySaveSuccess(tr("Change Reason '%1' saved").arg(code));
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

void ChangeReasonDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete change reason while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(reason_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Change Reason",
        QString("Are you sure you want to delete change reason '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting change reason: " << reason_.code;

    QPointer<ChangeReasonDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = reason_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_change_reason_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Change Reason deleted successfully";
            emit self->statusMessage(QString("Change Reason '%1' deleted").arg(code));
            emit self->reasonDeleted(code);
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
