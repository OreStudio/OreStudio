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
#include "ores.qt/PartyRoleTypeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/party_role_type_protocol.hpp"
#include "ui_PartyRoleTypeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

PartyRoleTypeDetailDialog::PartyRoleTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PartyRoleTypeDetailDialog)
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

PartyRoleTypeDetailDialog::~PartyRoleTypeDetailDialog() {
    delete ui_;
}

QTabWidget* PartyRoleTypeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PartyRoleTypeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PartyRoleTypeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PartyRoleTypeDetailDialog::code() const {
    return QString::fromStdString(party_role_type_.code);
}

void PartyRoleTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PartyRoleTypeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &PartyRoleTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &PartyRoleTypeDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &PartyRoleTypeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &PartyRoleTypeDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &PartyRoleTypeDetailDialog::onFieldChanged);
}

void PartyRoleTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PartyRoleTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyRoleTypeDetailDialog::setType(const trading::domain::party_role_type& party_role_type) {
    party_role_type_ = party_role_type;
    updateUiFromType();
}

void PartyRoleTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyRoleTypeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyRoleTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PartyRoleTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(party_role_type_.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(party_role_type_.description));

    populateProvenance(party_role_type_.version,
                       party_role_type_.modified_by,
                       party_role_type_.performed_by,
                       party_role_type_.recorded_at,
                       party_role_type_.change_reason_code,
                       party_role_type_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyRoleTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        party_role_type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    party_role_type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    party_role_type_.modified_by = username_;
}

void PartyRoleTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyRoleTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyRoleTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyRoleTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();

    return true && !code_val.isEmpty();
}

void PartyRoleTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save party role type while disconnected from server.");
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
    party_role_type_.change_reason_code = crSel->reason_code;
    party_role_type_.change_commentary = crSel->commentary;

    updateTypeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party role type: " << party_role_type_.code;

    QPointer<PartyRoleTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, party_role_type = party_role_type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_party_role_type_request request;
        request.data = party_role_type;
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
                    BOOST_LOG_SEV(lg(), info) << "Party Role Type saved successfully";
                    QString code = QString::fromStdString(self->party_role_type_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->party_role_typeSaved(code);
                    self->notifySaveSuccess(tr("Party Role Type '%1' saved").arg(code));
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

void PartyRoleTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete party role type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(party_role_type_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Party Role Type",
        QString("Are you sure you want to delete party role type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting party role type: " << party_role_type_.code;

    QPointer<PartyRoleTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = party_role_type_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_party_role_type_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Party Role Type deleted successfully";
            emit self->statusMessage(QString("Party Role Type '%1' deleted").arg(code));
            emit self->party_role_typeDeleted(code);
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
