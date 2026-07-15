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
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/crm_enabled_derived_pair_protocol.hpp"
#include "ui_CrmEnabledDerivedPairDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CrmEnabledDerivedPairDetailDialog::CrmEnabledDerivedPairDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CrmEnabledDerivedPairDetailDialog)
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
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
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

void CrmEnabledDerivedPairDetailDialog::setupCombos() {}

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
    connect(ui_->configCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->baseCcyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->quoteCcyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
    connect(ui_->enabledCheckBox,
            &QCheckBox::toggled,
            this,
            &CrmEnabledDerivedPairDetailDialog::onFieldChanged);
}

void CrmEnabledDerivedPairDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateConfigId();
    populateBaseCurrencyCodeCombo();
    populateQuoteCurrencyCodeCombo();
}

void CrmEnabledDerivedPairDetailDialog::populateBaseCurrencyCodeCombo() {
    setup_currency_combo(ui_->baseCcyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.base_currency_code);
    });
}

void CrmEnabledDerivedPairDetailDialog::populateQuoteCurrencyCodeCombo() {
    setup_currency_combo(ui_->quoteCcyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.quote_currency_code);
    });
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
    ui_->idEdit->setReadOnly(true);
    ui_->configCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        pair_.id = boost::uuids::random_generator()();
        if (clientManager_)
            pair_.party_id = clientManager_->currentPartyId();
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
    ui_->configCombo->setEnabled(false);
    ui_->baseCcyCombo->setEnabled(!readOnly);
    ui_->quoteCcyCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CrmEnabledDerivedPairDetailDialog::populateConfigId() {
    BOOST_LOG_SEV(lg(), debug) << "Populating config_id combo";
    populateDynamicCombo<refdata::domain::crm_topology_config>(
        ui_->configCombo,
        this,
        clientManager_,
        &fetch_crm_topology_configs,
        "crmEnabledDerivedPairConfigWatcher",
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return QString::fromStdString(t.pivot_currency_code); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(boost::uuids::to_string(pair_.config_id)); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load CRM topology configs: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(boost::uuids::to_string(t.id)); },
        [](const auto&) { return false; });
}
void CrmEnabledDerivedPairDetailDialog::updateUiFromPair() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(pair_.id)));
    {
        const auto val = QString::fromStdString(boost::uuids::to_string(pair_.config_id));
        const int idx = ui_->configCombo->findData(val);
        if (idx >= 0)
            ui_->configCombo->setCurrentIndex(idx);
    }
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

void CrmEnabledDerivedPairDetailDialog::updatePairFromUi() {
    if (createMode_) {
        const auto config_id_str =
            ui_->configCombo->currentData().toString().trimmed().toStdString();
        if (!config_id_str.empty()) {
            try {
                pair_.config_id = boost::uuids::string_generator()(config_id_str);
            } catch (const std::exception&) {
                // Leave the field unchanged; validation catches the empty/nil
                // selection at save time (no combo choice made yet).
            }
        }
    }
    pair_.base_currency_code = ui_->baseCcyCombo->currentText().toStdString();
    pair_.quote_currency_code = ui_->quoteCcyCombo->currentText().toStdString();
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
    const bool config_id_selected = ui_->configCombo->currentIndex() >= 0;
    const bool base_currency_code_selected = ui_->baseCcyCombo->currentIndex() >= 0;
    const bool quote_currency_code_selected = ui_->quoteCcyCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && config_id_selected && base_currency_code_selected &&
           quote_currency_code_selected;
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
