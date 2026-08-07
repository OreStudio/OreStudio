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
#include "ores.qt/CatalogDetailDialog.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_CatalogDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CatalogDetailDialog::CatalogDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CatalogDetailDialog)
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

CatalogDetailDialog::~CatalogDetailDialog() {
    delete ui_;
}

QTabWidget* CatalogDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CatalogDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CatalogDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CatalogDetailDialog::code() const {
    return QString::fromStdString(catalog_.name);
}

void CatalogDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CatalogDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CatalogDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &CatalogDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &CatalogDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &CatalogDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &CatalogDetailDialog::onFieldChanged);
    connect(ui_->ownerEdit, &QLineEdit::textChanged, this, &CatalogDetailDialog::onFieldChanged);
}

void CatalogDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CatalogDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CatalogDetailDialog::setCatalog(const dq::domain::catalog& catalog) {
    catalog_ = catalog;
    updateUiFromCatalog();
}

void CatalogDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CatalogDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CatalogDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->ownerEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CatalogDetailDialog::updateUiFromCatalog() {
    ui_->nameEdit->setText(QString::fromStdString(catalog_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(catalog_.description));
    ui_->ownerEdit->setText(catalog_.owner ? QString::fromStdString(*catalog_.owner) : QString{});

    populateProvenance(catalog_.version,
                       catalog_.modified_by,
                       catalog_.performed_by,
                       catalog_.recorded_at,
                       catalog_.change_reason_code,
                       catalog_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CatalogDetailDialog::updateCatalogFromUi() {
    if (createMode_) {
        catalog_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    catalog_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    {
        const auto owner_str = ui_->ownerEdit->text().trimmed().toStdString();
        catalog_.owner = owner_str.empty() ? std::nullopt : std::optional<std::string>(owner_str);
    }
    catalog_.modified_by = username_;
}

void CatalogDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CatalogDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CatalogDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CatalogDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !name_val.isEmpty();
}

void CatalogDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save catalog while disconnected from server.");
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
    catalog_.change_reason_code = crSel->reason_code;
    catalog_.change_commentary = crSel->commentary;

    updateCatalogFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving catalog: " << catalog_.name;

    QPointer<CatalogDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, catalog = catalog_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_catalog_request request;
        request.data = catalog;
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
                    BOOST_LOG_SEV(lg(), info) << "Catalog saved successfully";
                    QString code = QString::fromStdString(self->catalog_.name);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->catalogSaved(code);
                    self->notifySaveSuccess(tr("Catalog '%1' saved").arg(code));
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

void CatalogDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete catalog while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(catalog_.name);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Catalog",
        QString("Are you sure you want to delete catalog '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting catalog: " << catalog_.name;

    QPointer<CatalogDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = catalog_.name]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_catalog_request request;
        request.names = {code};
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
            BOOST_LOG_SEV(lg(), info) << "Catalog deleted successfully";
            emit self->statusMessage(QString("Catalog '%1' deleted").arg(code));
            emit self->catalogDeleted(code);
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
