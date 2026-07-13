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
#include "ores.qt/CrmDriverPairDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/crm_driver_pair_protocol.hpp"
#include "ui_CrmDriverPairDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CrmDriverPairDetailDialog::CrmDriverPairDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CrmDriverPairDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

CrmDriverPairDetailDialog::~CrmDriverPairDetailDialog() {
    delete ui_;
}

QTabWidget* CrmDriverPairDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CrmDriverPairDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CrmDriverPairDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CrmDriverPairDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(pair_.id));
}

void CrmDriverPairDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CrmDriverPairDetailDialog::setupCombos() {}

void CrmDriverPairDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &CrmDriverPairDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CrmDriverPairDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &CrmDriverPairDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &CrmDriverPairDetailDialog::onCodeChanged);
    connect(ui_->baseCcyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmDriverPairDetailDialog::onFieldChanged);
    connect(ui_->quoteCcyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmDriverPairDetailDialog::onFieldChanged);
    connect(ui_->enabledCheckBox,
            &QCheckBox::toggled,
            this,
            &CrmDriverPairDetailDialog::onFieldChanged);
}

void CrmDriverPairDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateBaseCurrencyCodeCombo();
    populateQuoteCurrencyCodeCombo();
}

void CrmDriverPairDetailDialog::populateBaseCurrencyCodeCombo() {
    setup_currency_combo(ui_->baseCcyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.base_currency_code);
    });
}

void CrmDriverPairDetailDialog::populateQuoteCurrencyCodeCombo() {
    setup_currency_combo(ui_->quoteCcyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.quote_currency_code);
    });
}

void CrmDriverPairDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CrmDriverPairDetailDialog::setPair(const refdata::domain::crm_driver_pair& pair) {
    pair_ = pair;
    updateUiFromPair();
}

void CrmDriverPairDetailDialog::setCreateMode(bool createMode) {
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

void CrmDriverPairDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmDriverPairDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->baseCcyCombo->setEnabled(!readOnly);
    ui_->quoteCcyCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CrmDriverPairDetailDialog::updateUiFromPair() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(pair_.id)));
    {
        const auto val = QString::fromStdString(pair_.base_currency_code);
        const int idx = ui_->baseCcyCombo->findText(val);
        ui_->baseCcyCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(pair_.quote_currency_code);
        const int idx = ui_->quoteCcyCombo->findText(val);
        ui_->quoteCcyCombo->setCurrentIndex(idx);
    }
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

void CrmDriverPairDetailDialog::updatePairFromUi() {
    pair_.base_currency_code = ui_->baseCcyCombo->currentText().toStdString();
    pair_.quote_currency_code = ui_->quoteCcyCombo->currentText().toStdString();
    pair_.enabled = ui_->enabledCheckBox->isChecked();
    pair_.modified_by = username_;
}

void CrmDriverPairDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmDriverPairDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmDriverPairDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CrmDriverPairDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const bool base_currency_code_selected = ui_->baseCcyCombo->currentIndex() >= 0;
    const bool quote_currency_code_selected = ui_->quoteCcyCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && base_currency_code_selected && quote_currency_code_selected;
}

void CrmDriverPairDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save CRM driver pair while disconnected from server.");
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

    BOOST_LOG_SEV(lg(), info) << "Saving CRM driver pair: " << boost::uuids::to_string(pair_.id);

    QPointer<CrmDriverPairDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, pair = pair_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_crm_driver_pair_request request;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair saved successfully";
            QString code = QString::fromStdString(boost::uuids::to_string(self->pair_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->pairSaved(code);
            self->notifySaveSuccess(tr("CRM Driver Pair '%1' saved").arg(code));
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

void CrmDriverPairDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete CRM driver pair while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(pair_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete CRM Driver Pair",
        QString("Are you sure you want to delete CRM driver pair '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting CRM driver pair: " << boost::uuids::to_string(pair_.id);

    QPointer<CrmDriverPairDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(pair_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_crm_driver_pair_request request;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Driver Pair deleted successfully";
            emit self->statusMessage(QString("CRM Driver Pair '%1' deleted").arg(code));
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
