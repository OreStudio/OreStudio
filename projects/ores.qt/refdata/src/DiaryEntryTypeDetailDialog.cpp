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
#include "ores.qt/DiaryEntryTypeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/diary_entry_type_protocol.hpp"
#include "ui_DiaryEntryTypeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

DiaryEntryTypeDetailDialog::DiaryEntryTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::DiaryEntryTypeDetailDialog)
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

DiaryEntryTypeDetailDialog::~DiaryEntryTypeDetailDialog() {
    delete ui_;
}

QTabWidget* DiaryEntryTypeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* DiaryEntryTypeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* DiaryEntryTypeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString DiaryEntryTypeDetailDialog::code() const {
    return QString::fromStdString(entry_type_.code);
}

void DiaryEntryTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void DiaryEntryTypeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &DiaryEntryTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &DiaryEntryTypeDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &DiaryEntryTypeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &DiaryEntryTypeDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &DiaryEntryTypeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &DiaryEntryTypeDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &DiaryEntryTypeDetailDialog::onFieldChanged);
}

void DiaryEntryTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void DiaryEntryTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void DiaryEntryTypeDetailDialog::setType(const refdata::domain::diary_entry_type& entry_type) {
    entry_type_ = entry_type;
    updateUiFromType();
}

void DiaryEntryTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void DiaryEntryTypeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DiaryEntryTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void DiaryEntryTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(entry_type_.code));
    ui_->nameEdit->setText(QString::fromStdString(entry_type_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(entry_type_.description));
    ui_->displayOrderEdit->setValue(entry_type_.display_order);

    populateProvenance(entry_type_.version,
                       entry_type_.modified_by,
                       entry_type_.performed_by,
                       entry_type_.recorded_at,
                       entry_type_.change_reason_code,
                       entry_type_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void DiaryEntryTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        entry_type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    entry_type_.name = ui_->nameEdit->text().trimmed().toStdString();
    entry_type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    entry_type_.display_order = ui_->displayOrderEdit->value();
    entry_type_.modified_by = username_;
}

void DiaryEntryTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DiaryEntryTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DiaryEntryTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool DiaryEntryTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void DiaryEntryTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save diary entry type while disconnected from server.");
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
    entry_type_.change_reason_code = crSel->reason_code;
    entry_type_.change_commentary = crSel->commentary;

    updateTypeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving diary entry type: " << entry_type_.code;

    QPointer<DiaryEntryTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, entry_type = entry_type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_diary_entry_type_request request;
        request.data = entry_type;
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
                    BOOST_LOG_SEV(lg(), info) << "Diary Entry Type saved successfully";
                    QString code = QString::fromStdString(self->entry_type_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->entry_typeSaved(code);
                    self->notifySaveSuccess(tr("Diary Entry Type '%1' saved").arg(code));
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

void DiaryEntryTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete diary entry type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(entry_type_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Diary Entry Type",
        QString("Are you sure you want to delete diary entry type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting diary entry type: " << entry_type_.code;

    QPointer<DiaryEntryTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = entry_type_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_diary_entry_type_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Diary Entry Type deleted successfully";
            emit self->statusMessage(QString("Diary Entry Type '%1' deleted").arg(code));
            emit self->entry_typeDeleted(code);
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
