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
#include "ores.qt/CrmTopologyConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/crm_topology_config_protocol.hpp"
#include "ui_CrmTopologyConfigDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CrmTopologyConfigDetailDialog::CrmTopologyConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CrmTopologyConfigDetailDialog)
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

CrmTopologyConfigDetailDialog::~CrmTopologyConfigDetailDialog() {
    delete ui_;
}

QTabWidget* CrmTopologyConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CrmTopologyConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CrmTopologyConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CrmTopologyConfigDetailDialog::code() const {
    return QString::fromStdString(config_.name);
}

void CrmTopologyConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CrmTopologyConfigDetailDialog::setupCombos() {}

void CrmTopologyConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CrmTopologyConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CrmTopologyConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CrmTopologyConfigDetailDialog::onCloseClicked);

    connect(
        ui_->idEdit, &QLineEdit::textChanged, this, &CrmTopologyConfigDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &CrmTopologyConfigDetailDialog::onFieldChanged);
    connect(ui_->pivotCcyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmTopologyConfigDetailDialog::onFieldChanged);
    connect(ui_->enabledCheckBox,
            &QCheckBox::toggled,
            this,
            &CrmTopologyConfigDetailDialog::onFieldChanged);
}

void CrmTopologyConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populatePivotCurrencyCodeCombo();
}

void CrmTopologyConfigDetailDialog::populatePivotCurrencyCodeCombo() {
    setup_currency_combo(ui_->pivotCcyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(config_.pivot_currency_code);
    });
}

void CrmTopologyConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CrmTopologyConfigDetailDialog::setConfig(const refdata::domain::crm_topology_config& config) {
    config_ = config;
    updateUiFromConfig();
}

void CrmTopologyConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        config_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CrmTopologyConfigDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmTopologyConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->pivotCcyCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CrmTopologyConfigDetailDialog::updateUiFromConfig() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(config_.id)));
    ui_->nameEdit->setText(QString::fromStdString(config_.name));
    {
        const auto val = QString::fromStdString(config_.pivot_currency_code);
        const int idx = ui_->pivotCcyCombo->findText(val);
        ui_->pivotCcyCombo->setCurrentIndex(idx);
    }
    ui_->enabledCheckBox->setChecked(config_.enabled);

    populateProvenance(config_.version,
                       config_.modified_by,
                       config_.performed_by,
                       config_.recorded_at,
                       config_.change_reason_code,
                       config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CrmTopologyConfigDetailDialog::updateConfigFromUi() {
    config_.name = ui_->nameEdit->text().trimmed().toStdString();
    config_.pivot_currency_code = ui_->pivotCcyCombo->currentText().toStdString();
    config_.enabled = ui_->enabledCheckBox->isChecked();
    config_.modified_by = username_;
}

void CrmTopologyConfigDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmTopologyConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CrmTopologyConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CrmTopologyConfigDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const bool pivot_currency_code_selected = ui_->pivotCcyCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && !name_val.isEmpty() && pivot_currency_code_selected;
}

void CrmTopologyConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save CRM topology config while disconnected from server.");
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
    config_.change_reason_code = crSel->reason_code;
    config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving CRM topology config: " << config_.name;

    QPointer<CrmTopologyConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, config = config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_crm_topology_config_request request;
        request.data = config;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Topology Config saved successfully";
            QString code = QString::fromStdString(self->config_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->configSaved(code);
            self->notifySaveSuccess(tr("CRM Topology Config '%1' saved").arg(code));
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

void CrmTopologyConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete CRM topology config while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(config_.name);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete CRM Topology Config",
        QString("Are you sure you want to delete CRM topology config '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting CRM topology config: " << config_.name;

    QPointer<CrmTopologyConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(config_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_crm_topology_config_request request;
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
            BOOST_LOG_SEV(lg(), info) << "CRM Topology Config deleted successfully";
            emit self->statusMessage(QString("CRM Topology Config '%1' deleted").arg(code));
            emit self->configDeleted(code);
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
