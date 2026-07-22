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
#include "ores.qt/PartyIdSchemeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ui_PartyIdSchemeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

PartyIdSchemeDetailDialog::PartyIdSchemeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PartyIdSchemeDetailDialog)
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

PartyIdSchemeDetailDialog::~PartyIdSchemeDetailDialog() {
    delete ui_;
}

QTabWidget* PartyIdSchemeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PartyIdSchemeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PartyIdSchemeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PartyIdSchemeDetailDialog::code() const {
    return QString::fromStdString(scheme_.code);
}

void PartyIdSchemeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PartyIdSchemeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &PartyIdSchemeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &PartyIdSchemeDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &PartyIdSchemeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &PartyIdSchemeDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &PartyIdSchemeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
    connect(ui_->codingSchemeCodeEdit,
            &QLineEdit::textChanged,
            this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
    connect(ui_->maxCardinalityEdit,
            &QSpinBox::valueChanged,
            this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
}

void PartyIdSchemeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PartyIdSchemeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyIdSchemeDetailDialog::setScheme(const refdata::domain::party_id_scheme& scheme) {
    scheme_ = scheme;
    updateUiFromScheme();
}

void PartyIdSchemeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->codingSchemeCodeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PartyIdSchemeDetailDialog::updateUiFromScheme() {
    ui_->codeEdit->setText(QString::fromStdString(scheme_.code));
    ui_->nameEdit->setText(QString::fromStdString(scheme_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(scheme_.description));
    ui_->codingSchemeCodeEdit->setText(QString::fromStdString(scheme_.coding_scheme_code));
    ui_->displayOrderEdit->setValue(scheme_.display_order);
    ui_->maxCardinalityEdit->setValue(
        scheme_.max_cardinality.value_or(ui_->maxCardinalityEdit->minimum()));

    populateProvenance(scheme_.version,
                       scheme_.modified_by,
                       scheme_.performed_by,
                       scheme_.recorded_at,
                       scheme_.change_reason_code,
                       scheme_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::updateSchemeFromUi() {
    if (createMode_) {
        scheme_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    scheme_.name = ui_->nameEdit->text().trimmed().toStdString();
    scheme_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    scheme_.coding_scheme_code = ui_->codingSchemeCodeEdit->text().trimmed().toStdString();
    scheme_.display_order = ui_->displayOrderEdit->value();
    if (ui_->maxCardinalityEdit->value() == ui_->maxCardinalityEdit->minimum())
        scheme_.max_cardinality = std::nullopt;
    else
        scheme_.max_cardinality = ui_->maxCardinalityEdit->value();
    scheme_.modified_by = username_;
}

void PartyIdSchemeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyIdSchemeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void PartyIdSchemeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save party ID scheme while disconnected from server.");
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
    scheme_.change_reason_code = crSel->reason_code;
    scheme_.change_commentary = crSel->commentary;

    updateSchemeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party ID scheme: " << scheme_.code;

    QPointer<PartyIdSchemeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, scheme = scheme_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_party_id_scheme_request request;
        request.data = scheme;
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
                    BOOST_LOG_SEV(lg(), info) << "Party ID Scheme saved successfully";
                    QString code = QString::fromStdString(self->scheme_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->schemeSaved(code);
                    self->notifySaveSuccess(tr("Party ID Scheme '%1' saved").arg(code));
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

void PartyIdSchemeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete party ID scheme while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(scheme_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Party ID Scheme",
        QString("Are you sure you want to delete party ID scheme '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting party ID scheme: " << scheme_.code;

    QPointer<PartyIdSchemeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = scheme_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_party_id_scheme_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Party ID Scheme deleted successfully";
            emit self->statusMessage(QString("Party ID Scheme '%1' deleted").arg(code));
            emit self->schemeDeleted(code);
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
