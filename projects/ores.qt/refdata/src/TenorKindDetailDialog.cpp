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
#include "ores.qt/TenorKindDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/tenor_kind_protocol.hpp"
#include "ui_TenorKindDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

TenorKindDetailDialog::TenorKindDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::TenorKindDetailDialog)
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

TenorKindDetailDialog::~TenorKindDetailDialog() {
    delete ui_;
}

QTabWidget* TenorKindDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* TenorKindDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* TenorKindDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString TenorKindDetailDialog::code() const {
    return QString::fromStdString(kind_.code);
}

void TenorKindDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenorKindDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &TenorKindDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &TenorKindDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &TenorKindDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &TenorKindDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &TenorKindDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &TenorKindDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &TenorKindDetailDialog::onFieldChanged);
}

void TenorKindDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void TenorKindDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenorKindDetailDialog::setKind(const refdata::domain::tenor_kind& kind) {
    kind_ = kind;
    updateUiFromKind();
}

void TenorKindDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorKindDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorKindDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenorKindDetailDialog::updateUiFromKind() {
    ui_->codeEdit->setText(QString::fromStdString(kind_.code));
    ui_->nameEdit->setText(QString::fromStdString(kind_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(kind_.description));
    ui_->displayOrderEdit->setValue(kind_.display_order);

    populateProvenance(kind_.version,
                       kind_.modified_by,
                       kind_.performed_by,
                       kind_.recorded_at,
                       kind_.change_reason_code,
                       kind_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorKindDetailDialog::updateKindFromUi() {
    if (createMode_) {
        kind_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    kind_.name = ui_->nameEdit->text().trimmed().toStdString();
    kind_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    kind_.display_order = ui_->displayOrderEdit->value();
    kind_.modified_by = username_;
}

void TenorKindDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorKindDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorKindDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenorKindDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void TenorKindDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save tenor kind while disconnected from server.");
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
    kind_.change_reason_code = crSel->reason_code;
    kind_.change_commentary = crSel->commentary;

    updateKindFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenor kind: " << kind_.code;

    QPointer<TenorKindDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, kind = kind_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_tenor_kind_request request;
        request.data = kind;
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
                    BOOST_LOG_SEV(lg(), info) << "Tenor Kind saved successfully";
                    QString code = QString::fromStdString(self->kind_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->kindSaved(code);
                    self->notifySaveSuccess(tr("Tenor Kind '%1' saved").arg(code));
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

void TenorKindDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete tenor kind while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(kind_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Tenor Kind",
        QString("Are you sure you want to delete tenor kind '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting tenor kind: " << kind_.code;

    QPointer<TenorKindDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = kind_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_tenor_kind_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Tenor Kind deleted successfully";
            emit self->statusMessage(QString("Tenor Kind '%1' deleted").arg(code));
            emit self->kindDeleted(code);
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
