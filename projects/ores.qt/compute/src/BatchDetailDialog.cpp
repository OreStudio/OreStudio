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
#include "ores.qt/BatchDetailDialog.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_BatchDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

BatchDetailDialog::BatchDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::BatchDetailDialog)
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

BatchDetailDialog::~BatchDetailDialog() {
    delete ui_;
}

QTabWidget* BatchDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BatchDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BatchDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString BatchDetailDialog::code() const {
    return QString::fromStdString(batch_.external_ref);
}

void BatchDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BatchDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &BatchDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &BatchDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &BatchDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &BatchDetailDialog::onCodeChanged);
}

void BatchDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BatchDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BatchDetailDialog::setBatch(const compute::domain::batch& batch) {
    batch_ = batch;
    updateUiFromBatch();
}

void BatchDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        batch_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void BatchDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BatchDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BatchDetailDialog::updateUiFromBatch() {
    ui_->codeEdit->setText(QString::fromStdString(batch_.external_ref));

    populateProvenance(batch_.version,
                       batch_.modified_by,
                       batch_.performed_by,
                       batch_.recorded_at,
                       batch_.change_reason_code,
                       batch_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BatchDetailDialog::updateBatchFromUi() {
    if (createMode_) {
        batch_.external_ref = ui_->codeEdit->text().trimmed().toStdString();
    }
    batch_.modified_by = username_;
}

void BatchDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BatchDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BatchDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BatchDetailDialog::validateInput() {
    const QString external_ref_val = ui_->codeEdit->text().trimmed();

    return true && !external_ref_val.isEmpty();
}

void BatchDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save compute batch while disconnected from server.");
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
    batch_.change_reason_code = crSel->reason_code;
    batch_.change_commentary = crSel->commentary;

    updateBatchFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving compute batch: " << batch_.external_ref;

    QPointer<BatchDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, batch = batch_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::save_batch_request request;
        request.data = batch;
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
                    BOOST_LOG_SEV(lg(), info) << "Batch saved successfully";
                    QString code = QString::fromStdString(self->batch_.external_ref);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->batchSaved(code);
                    self->notifySaveSuccess(tr("Batch '%1' saved").arg(code));
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

void BatchDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete compute batch while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(batch_.external_ref);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Batch",
        QString("Are you sure you want to delete compute batch '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting compute batch: " << batch_.external_ref;

    QPointer<BatchDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(batch_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::delete_batch_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Batch deleted successfully";
            emit self->statusMessage(QString("Batch '%1' deleted").arg(code));
            emit self->batchDeleted(code);
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
