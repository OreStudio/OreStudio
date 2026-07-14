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
#include "ores.qt/AssetClassCodeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/asset_class_code_protocol.hpp"
#include "ui_AssetClassCodeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

AssetClassCodeDetailDialog::AssetClassCodeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::AssetClassCodeDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

AssetClassCodeDetailDialog::~AssetClassCodeDetailDialog() {
    delete ui_;
}

QTabWidget* AssetClassCodeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* AssetClassCodeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* AssetClassCodeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString AssetClassCodeDetailDialog::code() const {
    return QString::fromStdString(class__.code);
}

void AssetClassCodeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void AssetClassCodeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &AssetClassCodeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &AssetClassCodeDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &AssetClassCodeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &AssetClassCodeDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &AssetClassCodeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &AssetClassCodeDetailDialog::onFieldChanged);
}

void AssetClassCodeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AssetClassCodeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void AssetClassCodeDetailDialog::setCode(const refdata::domain::asset_class_code& class_) {
    class__ = class_;
    updateUiFromCode();
}

void AssetClassCodeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void AssetClassCodeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AssetClassCodeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void AssetClassCodeDetailDialog::updateUiFromCode() {
    ui_->codeEdit->setText(QString::fromStdString(class__.code));
    ui_->nameEdit->setText(QString::fromStdString(class__.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(class__.description));
    ui_->displayOrderEdit->setValue(class__.display_order);

    populateProvenance(class__.version,
                       class__.modified_by,
                       class__.performed_by,
                       class__.recorded_at,
                       class__.change_reason_code,
                       class__.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void AssetClassCodeDetailDialog::updateCodeFromUi() {
    if (createMode_) {
        class__.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    class__.name = ui_->nameEdit->text().trimmed().toStdString();
    class__.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    class__.display_order = ui_->displayOrderEdit->value();
    class__.modified_by = username_;
}

void AssetClassCodeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AssetClassCodeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AssetClassCodeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool AssetClassCodeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void AssetClassCodeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save asset class code while disconnected from server.");
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
    class__.change_reason_code = crSel->reason_code;
    class__.change_commentary = crSel->commentary;

    updateCodeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving asset class code: " << class__.code;

    QPointer<AssetClassCodeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, class_ = class__]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_asset_class_code_request request;
        request.data = class_;
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
            BOOST_LOG_SEV(lg(), info) << "Asset Class Code saved successfully";
            QString code = QString::fromStdString(self->class__.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->class_Saved(code);
            self->notifySaveSuccess(tr("Asset Class Code '%1' saved").arg(code));
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

void AssetClassCodeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete asset class code while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(class__.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Asset Class Code",
        QString("Are you sure you want to delete asset class code '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting asset class code: " << class__.code;

    QPointer<AssetClassCodeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = class__.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_asset_class_code_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Asset Class Code deleted successfully";
            emit self->statusMessage(QString("Asset Class Code '%1' deleted").arg(code));
            emit self->class_Deleted(code);
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
