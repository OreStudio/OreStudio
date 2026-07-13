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
#include "ores.qt/CrmEnabledDerivedPairDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/crm_enabled_derived_pair_protocol.hpp"
#include "ui_CrmEnabledDerivedPairDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CrmEnabledDerivedPairDetailDialog::CrmEnabledDerivedPairDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CrmEnabledDerivedPairDetailDialog)
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

CrmEnabledDerivedPairDetailDialog::~CrmEnabledDerivedPairDetailDialog() {
    delete ui_;
}

QTabWidget* CrmEnabledDerivedPairDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CrmEnabledDerivedPairDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CrmEnabledDerivedPairDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CrmEnabledDerivedPairDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(pair_.id));
}

void CrmEnabledDerivedPairDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CrmEnabledDerivedPairDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CrmEnabledDerivedPairDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CrmEnabledDerivedPairDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CrmEnabledDerivedPairDetailDialog::onCloseClicked);

    connect(ui_->idEdit,
            &QLineEdit::textChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onCodeChanged);
    connect(ui_->partyIdEdit,
            &QLineEdit::textChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->configIdEdit,
            &QLineEdit::textChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->baseCcyEdit,
            &QLineEdit::textChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->quoteCcyEdit,
            &QLineEdit::textChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->enabledCheckBox,
            &QCheckBox::toggled,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
}

void CrmEnabledDerivedPairDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CrmEnabledDerivedPairDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CrmEnabledDerivedPairDetailDialog::setPair(
    const refdata::domain::crm_enabled_derived_pair& pair) {
    pair_ = pair;
    updateUiFromPair();
}

void CrmEnabledDerivedPairDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        pair_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CrmEnabledDerivedPairDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmEnabledDerivedPairDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->partyIdEdit->setReadOnly(readOnly);
    ui_->configIdEdit->setReadOnly(readOnly);
    ui_->baseCcyEdit->setReadOnly(readOnly);
    ui_->quoteCcyEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CrmEnabledDerivedPairDetailDialog::updateUiFromPair() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(pair_.id)));
    ui_->partyIdEdit->setText(QString::fromStdString(boost::uuids::to_string(pair_.party_id)));
    ui_->configIdEdit->setText(QString::fromStdString(boost::uuids::to_string(pair_.config_id)));
    ui_->baseCcyEdit->setText(QString::fromStdString(pair_.base_currency_code));
    ui_->quoteCcyEdit->setText(QString::fromStdString(pair_.quote_currency_code));
    ui_->enabledCheckBox->setChecked(pair_.enabled);

    populateProvenance(pair_.version,
                       pair_.modified_by,
                       pair_.performed_by,
                       pair_.recorded_at,
                       pair_.change_reason_code,
                       pair_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CrmEnabledDerivedPairDetailDialog::updatePairFromUi() {
    pair_.base_currency_code = ui_->baseCcyEdit->text().trimmed().toStdString();
    pair_.quote_currency_code = ui_->quoteCcyEdit->text().trimmed().toStdString();
    pair_.enabled = ui_->enabledCheckBox->isChecked();
    pair_.modified_by = username_;
}

void CrmEnabledDerivedPairDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmEnabledDerivedPairDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmEnabledDerivedPairDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CrmEnabledDerivedPairDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString party_id_val = ui_->partyIdEdit->text().trimmed();
    const QString config_id_val = ui_->configIdEdit->text().trimmed();
    const QString base_currency_code_val = ui_->baseCcyEdit->text().trimmed();
    const QString quote_currency_code_val = ui_->quoteCcyEdit->text().trimmed();

    return true && !id_val.isEmpty() && !party_id_val.isEmpty() && !config_id_val.isEmpty() &&
           !base_currency_code_val.isEmpty() && !quote_currency_code_val.isEmpty();
}

void CrmEnabledDerivedPairDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save CRM enabled derived pair while disconnected from server.");
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
    pair_.change_reason_code = crSel->reason_code;
    pair_.change_commentary = crSel->commentary;

    updatePairFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving CRM enabled derived pair: "
                              << boost::uuids::to_string(pair_.id);

    QPointer<CrmEnabledDerivedPairDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, pair = pair_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_crm_enabled_derived_pair_request request;
        request.data = pair;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Enabled Derived Pair saved successfully";
            QString code = QString::fromStdString(boost::uuids::to_string(self->pair_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->pairSaved(code);
            self->notifySaveSuccess(tr("CRM Enabled Derived Pair '%1' saved").arg(code));
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

void CrmEnabledDerivedPairDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete CRM enabled derived pair while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(pair_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete CRM Enabled Derived Pair",
        QString("Are you sure you want to delete CRM enabled derived pair '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting CRM enabled derived pair: "
                              << boost::uuids::to_string(pair_.id);

    QPointer<CrmEnabledDerivedPairDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(pair_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_crm_enabled_derived_pair_request request;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Enabled Derived Pair deleted successfully";
            emit self->statusMessage(QString("CRM Enabled Derived Pair '%1' deleted").arg(code));
            emit self->pairDeleted(code);
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
