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
#include "ores.qt/WorkunitDetailDialog.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_WorkunitDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

WorkunitDetailDialog::WorkunitDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::WorkunitDetailDialog)
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

WorkunitDetailDialog::~WorkunitDetailDialog() {
    delete ui_;
}

QTabWidget* WorkunitDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* WorkunitDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* WorkunitDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString WorkunitDetailDialog::code() const {
    return QString::fromStdString(workunit_.input_uri);
}

void WorkunitDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void WorkunitDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &WorkunitDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &WorkunitDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &WorkunitDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &WorkunitDetailDialog::onCodeChanged);
}

void WorkunitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void WorkunitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void WorkunitDetailDialog::setWorkunit(const compute::domain::workunit& workunit) {
    workunit_ = workunit;
    updateUiFromWorkunit();
}

void WorkunitDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        workunit_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkunitDetailDialog::updateUiFromWorkunit() {
    ui_->codeEdit->setText(QString::fromStdString(workunit_.input_uri));

    populateProvenance(workunit_.version,
                       workunit_.modified_by,
                       workunit_.performed_by,
                       workunit_.recorded_at,
                       workunit_.change_reason_code,
                       workunit_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateWorkunitFromUi() {
    if (createMode_) {
        workunit_.input_uri = ui_->codeEdit->text().trimmed().toStdString();
    }
    workunit_.modified_by = username_;
}

void WorkunitDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool WorkunitDetailDialog::validateInput() {
    const QString input_uri_val = ui_->codeEdit->text().trimmed();

    return true && !input_uri_val.isEmpty();
}

void WorkunitDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save workunit while disconnected from server.");
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
    workunit_.change_reason_code = crSel->reason_code;
    workunit_.change_commentary = crSel->commentary;

    updateWorkunitFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving workunit: " << workunit_.input_uri;

    QPointer<WorkunitDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, workunit = workunit_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::save_workunit_request request;
        request.data = workunit;
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
                    BOOST_LOG_SEV(lg(), info) << "Workunit saved successfully";
                    QString code = QString::fromStdString(self->workunit_.input_uri);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->workunitSaved(code);
                    self->notifySaveSuccess(tr("Workunit '%1' saved").arg(code));
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

void WorkunitDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete workunit while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(workunit_.input_uri);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Workunit",
        QString("Are you sure you want to delete workunit '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting workunit: " << workunit_.input_uri;

    QPointer<WorkunitDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(workunit_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::delete_workunit_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Workunit deleted successfully";
            emit self->statusMessage(QString("Workunit '%1' deleted").arg(code));
            emit self->workunitDeleted(code);
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
