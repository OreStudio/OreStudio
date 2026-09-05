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
#include "ores.qt/HostDetailDialog.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_HostDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

HostDetailDialog::HostDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::HostDetailDialog)
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

HostDetailDialog::~HostDetailDialog() {
    delete ui_;
}

QTabWidget* HostDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* HostDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* HostDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString HostDetailDialog::code() const {
    return QString::fromStdString(host_.external_id);
}

void HostDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void HostDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &HostDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &HostDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &HostDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &HostDetailDialog::onCodeChanged);
}

void HostDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void HostDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void HostDetailDialog::setHost(const compute::domain::host& host) {
    host_ = host;
    updateUiFromHost();
}

void HostDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        host_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void HostDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void HostDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void HostDetailDialog::updateUiFromHost() {
    ui_->codeEdit->setText(QString::fromStdString(host_.external_id));

    populateProvenance(host_.version,
                       host_.modified_by,
                       host_.performed_by,
                       host_.recorded_at,
                       host_.change_reason_code,
                       host_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void HostDetailDialog::updateHostFromUi() {
    if (createMode_) {
        host_.external_id = ui_->codeEdit->text().trimmed().toStdString();
    }
    host_.modified_by = username_;
}

void HostDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void HostDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void HostDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool HostDetailDialog::validateInput() {
    const QString external_id_val = ui_->codeEdit->text().trimmed();

    return true && !external_id_val.isEmpty();
}

void HostDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save compute host while disconnected from server.");
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
    host_.change_reason_code = crSel->reason_code;
    host_.change_commentary = crSel->commentary;

    updateHostFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving compute host: " << host_.external_id;

    QPointer<HostDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, host = host_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::save_host_request request;
        request.data = host;
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
                    BOOST_LOG_SEV(lg(), info) << "Host saved successfully";
                    QString code = QString::fromStdString(self->host_.external_id);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->hostSaved(code);
                    self->notifySaveSuccess(tr("Host '%1' saved").arg(code));
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

void HostDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete compute host while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(host_.external_id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Host",
        QString("Are you sure you want to delete compute host '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting compute host: " << host_.external_id;

    QPointer<HostDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(host_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::delete_host_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Host deleted successfully";
            emit self->statusMessage(QString("Host '%1' deleted").arg(code));
            emit self->hostDeleted(code);
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
