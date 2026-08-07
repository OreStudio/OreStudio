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
#include "ores.qt/ChangeReasonCategoryDetailDialog.hpp"
#include "ores.dq.api/messaging/change_reason_category_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_ChangeReasonCategoryDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

ChangeReasonCategoryDetailDialog::ChangeReasonCategoryDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::ChangeReasonCategoryDetailDialog)
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

ChangeReasonCategoryDetailDialog::~ChangeReasonCategoryDetailDialog() {
    delete ui_;
}

QTabWidget* ChangeReasonCategoryDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ChangeReasonCategoryDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ChangeReasonCategoryDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString ChangeReasonCategoryDetailDialog::code() const {
    return QString::fromStdString(category_.code);
}

void ChangeReasonCategoryDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ChangeReasonCategoryDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &ChangeReasonCategoryDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &ChangeReasonCategoryDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &ChangeReasonCategoryDetailDialog::onCloseClicked);

    connect(ui_->codeEdit,
            &QLineEdit::textChanged,
            this,
            &ChangeReasonCategoryDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &ChangeReasonCategoryDetailDialog::onFieldChanged);
}

void ChangeReasonCategoryDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ChangeReasonCategoryDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ChangeReasonCategoryDetailDialog::setCategory(
    const dq::domain::change_reason_category& category) {
    category_ = category;
    updateUiFromCategory();
}

void ChangeReasonCategoryDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ChangeReasonCategoryDetailDialog::updateUiFromCategory() {
    ui_->codeEdit->setText(QString::fromStdString(category_.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(category_.description));

    populateProvenance(category_.version,
                       category_.modified_by,
                       category_.performed_by,
                       category_.recorded_at,
                       category_.change_reason_code,
                       category_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::updateCategoryFromUi() {
    if (createMode_) {
        category_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    category_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    category_.modified_by = username_;
}

void ChangeReasonCategoryDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ChangeReasonCategoryDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();

    return true && !code_val.isEmpty();
}

void ChangeReasonCategoryDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save change reason category while disconnected from server.");
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
    category_.change_reason_code = crSel->reason_code;
    category_.change_commentary = crSel->commentary;

    updateCategoryFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving change reason category: " << category_.code;

    QPointer<ChangeReasonCategoryDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, category = category_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_change_reason_category_request request;
        request.data = category;
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
                    BOOST_LOG_SEV(lg(), info) << "Change Reason Category saved successfully";
                    QString code = QString::fromStdString(self->category_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->categorySaved(code);
                    self->notifySaveSuccess(tr("Change Reason Category '%1' saved").arg(code));
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

void ChangeReasonCategoryDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete change reason category while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(category_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Change Reason Category",
        QString("Are you sure you want to delete change reason category '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting change reason category: " << category_.code;

    QPointer<ChangeReasonCategoryDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = category_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_change_reason_category_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Change Reason Category deleted successfully";
            emit self->statusMessage(QString("Change Reason Category '%1' deleted").arg(code));
            emit self->categoryDeleted(code);
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
