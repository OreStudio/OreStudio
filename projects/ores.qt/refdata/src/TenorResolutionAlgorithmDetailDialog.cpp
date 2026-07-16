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
#include "ores.qt/TenorResolutionAlgorithmDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/tenor_resolution_algorithm_protocol.hpp"
#include "ui_TenorResolutionAlgorithmDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

TenorResolutionAlgorithmDetailDialog::TenorResolutionAlgorithmDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::TenorResolutionAlgorithmDetailDialog)
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

TenorResolutionAlgorithmDetailDialog::~TenorResolutionAlgorithmDetailDialog() {
    delete ui_;
}

QTabWidget* TenorResolutionAlgorithmDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* TenorResolutionAlgorithmDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* TenorResolutionAlgorithmDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString TenorResolutionAlgorithmDetailDialog::code() const {
    return QString::fromStdString(algorithm_.code);
}

void TenorResolutionAlgorithmDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenorResolutionAlgorithmDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &TenorResolutionAlgorithmDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &TenorResolutionAlgorithmDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &TenorResolutionAlgorithmDetailDialog::onCloseClicked);

    connect(ui_->codeEdit,
            &QLineEdit::textChanged,
            this,
            &TenorResolutionAlgorithmDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &TenorResolutionAlgorithmDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &TenorResolutionAlgorithmDetailDialog::onFieldChanged);
}

void TenorResolutionAlgorithmDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void TenorResolutionAlgorithmDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenorResolutionAlgorithmDetailDialog::setAlgorithm(
    const refdata::domain::tenor_resolution_algorithm& algorithm) {
    algorithm_ = algorithm;
    updateUiFromAlgorithm();
}

void TenorResolutionAlgorithmDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorResolutionAlgorithmDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorResolutionAlgorithmDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenorResolutionAlgorithmDetailDialog::updateUiFromAlgorithm() {
    ui_->codeEdit->setText(QString::fromStdString(algorithm_.code));
    ui_->nameEdit->setText(QString::fromStdString(algorithm_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(algorithm_.description));
    ui_->displayOrderEdit->setValue(algorithm_.display_order);

    populateProvenance(algorithm_.version,
                       algorithm_.modified_by,
                       algorithm_.performed_by,
                       algorithm_.recorded_at,
                       algorithm_.change_reason_code,
                       algorithm_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorResolutionAlgorithmDetailDialog::updateAlgorithmFromUi() {
    if (createMode_) {
        algorithm_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    algorithm_.name = ui_->nameEdit->text().trimmed().toStdString();
    algorithm_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    algorithm_.display_order = ui_->displayOrderEdit->value();
    algorithm_.modified_by = username_;
}

void TenorResolutionAlgorithmDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorResolutionAlgorithmDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorResolutionAlgorithmDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenorResolutionAlgorithmDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void TenorResolutionAlgorithmDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save tenor resolution algorithm while disconnected from server.");
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
    algorithm_.change_reason_code = crSel->reason_code;
    algorithm_.change_commentary = crSel->commentary;

    updateAlgorithmFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenor resolution algorithm: " << algorithm_.code;

    QPointer<TenorResolutionAlgorithmDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, algorithm = algorithm_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_tenor_resolution_algorithm_request request;
        request.data = algorithm;
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
            BOOST_LOG_SEV(lg(), info) << "Tenor Resolution Algorithm saved successfully";
            QString code = QString::fromStdString(self->algorithm_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->algorithmSaved(code);
            self->notifySaveSuccess(tr("Tenor Resolution Algorithm '%1' saved").arg(code));
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

void TenorResolutionAlgorithmDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete tenor resolution algorithm while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(algorithm_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Tenor Resolution Algorithm",
        QString("Are you sure you want to delete tenor resolution algorithm '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting tenor resolution algorithm: " << algorithm_.code;

    QPointer<TenorResolutionAlgorithmDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = algorithm_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_tenor_resolution_algorithm_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Tenor Resolution Algorithm deleted successfully";
            emit self->statusMessage(QString("Tenor Resolution Algorithm '%1' deleted").arg(code));
            emit self->algorithmDeleted(code);
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
