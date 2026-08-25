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
#include "ores.qt/ResultDetailDialog.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_ResultDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

ResultDetailDialog::ResultDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::ResultDetailDialog)
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

ResultDetailDialog::~ResultDetailDialog() {
    delete ui_;
}

QTabWidget* ResultDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ResultDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ResultDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString ResultDetailDialog::code() const {
    return QString::fromStdString(result_.modified_by);
}

void ResultDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void ResultDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &ResultDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &ResultDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &ResultDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &ResultDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &ResultDetailDialog::onFieldChanged);
}

void ResultDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ResultDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void ResultDetailDialog::setResult(const compute::domain::result& result) {
    result_ = result;
    updateUiFromResult();
}

void ResultDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        result_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void ResultDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ResultDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ResultDetailDialog::updateUiFromResult() {
    ui_->codeEdit->setText(QString::fromStdString(result_.modified_by));
    ui_->nameEdit->setText(QString::fromStdString(result_.name));

    populateProvenance(result_.version,
                       result_.modified_by,
                       result_.performed_by,
                       result_.recorded_at,
                       result_.change_reason_code,
                       result_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ResultDetailDialog::updateResultFromUi() {
    if (createMode_) {
        result_.modified_by = ui_->codeEdit->text().trimmed().toStdString();
    }
    result_.name = ui_->nameEdit->text().trimmed().toStdString();
    result_.modified_by = username_;
}

void ResultDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ResultDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ResultDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool ResultDetailDialog::validateInput() {
    const QString modified_by_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !modified_by_val.isEmpty() && !name_val.isEmpty();
}

void ResultDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save compute result while disconnected from server.");
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
    result_.change_reason_code = crSel->reason_code;
    result_.change_commentary = crSel->commentary;

    updateResultFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving compute result: " << result_.modified_by;

    QPointer<ResultDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, result = result_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::save_result_request request;
        request.data = result;
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
                    BOOST_LOG_SEV(lg(), info) << "Result saved successfully";
                    QString code = QString::fromStdString(self->result_.modified_by);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->resultSaved(code);
                    self->notifySaveSuccess(tr("Result '%1' saved").arg(code));
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

void ResultDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete compute result while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(result_.modified_by);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Result",
        QString("Are you sure you want to delete compute result '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting compute result: " << result_.modified_by;

    QPointer<ResultDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(result_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        compute::messaging::delete_result_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Result deleted successfully";
            emit self->statusMessage(QString("Result '%1' deleted").arg(code));
            emit self->resultDeleted(code);
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
