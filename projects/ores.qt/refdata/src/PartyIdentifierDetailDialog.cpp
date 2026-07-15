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
#include "ores.qt/PartyIdentifierDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ui_PartyIdentifierDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

PartyIdentifierDetailDialog::PartyIdentifierDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PartyIdentifierDetailDialog)
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

PartyIdentifierDetailDialog::~PartyIdentifierDetailDialog() {
    delete ui_;
}

QTabWidget* PartyIdentifierDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PartyIdentifierDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PartyIdentifierDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PartyIdentifierDetailDialog::code() const {
    return QString::fromStdString(partyIdentifier_.id_value);
}

void PartyIdentifierDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PartyIdentifierDetailDialog::setupCombos() {}

void PartyIdentifierDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &PartyIdentifierDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &PartyIdentifierDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &PartyIdentifierDetailDialog::onCloseClicked);

    connect(
        ui_->idEdit, &QLineEdit::textChanged, this, &PartyIdentifierDetailDialog::onCodeChanged);
    connect(ui_->idSchemeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PartyIdentifierDetailDialog::onFieldChanged);
    connect(ui_->idValueEdit,
            &QLineEdit::textChanged,
            this,
            &PartyIdentifierDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QLineEdit::textChanged,
            this,
            &PartyIdentifierDetailDialog::onFieldChanged);
}

void PartyIdentifierDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateIdSchemeCombo();
}

void PartyIdentifierDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyIdentifierDetailDialog::setIdentifier(
    const refdata::domain::party_identifier& partyIdentifier) {
    partyIdentifier_ = partyIdentifier;
    updateUiFromIdentifier();
}

void PartyIdentifierDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        partyIdentifier_.id = boost::uuids::random_generator()();
        if (clientManager_)
            partyIdentifier_.party_id = clientManager_->currentPartyId();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyIdentifierDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdentifierDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->idSchemeCombo->setEnabled(!readOnly);
    ui_->idValueEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PartyIdentifierDetailDialog::populateIdSchemeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating id_scheme combo";
    populateDynamicCombo<refdata::domain::party_id_scheme>(
        ui_->idSchemeCombo,
        this,
        clientManager_,
        &fetch_party_id_schemes,
        "idSchemeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(partyIdentifier_.id_scheme); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load id schemes: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void PartyIdentifierDetailDialog::updateUiFromIdentifier() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(partyIdentifier_.id)));
    {
        const auto val = QString::fromStdString(partyIdentifier_.id_scheme);
        const int idx = ui_->idSchemeCombo->findData(val);
        if (idx >= 0)
            ui_->idSchemeCombo->setCurrentIndex(idx);
    }
    ui_->idValueEdit->setText(QString::fromStdString(partyIdentifier_.id_value));
    ui_->descriptionEdit->setText(QString::fromStdString(partyIdentifier_.description));

    populateProvenance(partyIdentifier_.version,
                       partyIdentifier_.modified_by,
                       partyIdentifier_.performed_by,
                       partyIdentifier_.recorded_at,
                       partyIdentifier_.change_reason_code,
                       partyIdentifier_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyIdentifierDetailDialog::updateIdentifierFromUi() {
    partyIdentifier_.id_scheme = ui_->idSchemeCombo->currentText().toStdString();
    partyIdentifier_.id_value = ui_->idValueEdit->text().trimmed().toStdString();
    partyIdentifier_.description = ui_->descriptionEdit->text().trimmed().toStdString();
    partyIdentifier_.modified_by = username_;
}

void PartyIdentifierDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdentifierDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdentifierDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyIdentifierDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString id_value_val = ui_->idValueEdit->text().trimmed();
    const bool id_scheme_selected = ui_->idSchemeCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && !id_value_val.isEmpty() && id_scheme_selected;
}

void PartyIdentifierDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save party identifier while disconnected from server.");
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
    partyIdentifier_.change_reason_code = crSel->reason_code;
    partyIdentifier_.change_commentary = crSel->commentary;

    updateIdentifierFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party identifier: " << partyIdentifier_.id_value;

    QPointer<PartyIdentifierDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, partyIdentifier = partyIdentifier_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_party_identifier_request request;
        request.data = partyIdentifier;
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
            BOOST_LOG_SEV(lg(), info) << "Party Identifier saved successfully";
            QString code = QString::fromStdString(self->partyIdentifier_.id_value);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->partyIdentifierSaved(code);
            self->notifySaveSuccess(tr("Party Identifier '%1' saved").arg(code));
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

void PartyIdentifierDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete party identifier while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(partyIdentifier_.id_value);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Party Identifier",
        QString("Are you sure you want to delete party identifier '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting party identifier: " << partyIdentifier_.id_value;

    QPointer<PartyIdentifierDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(partyIdentifier_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_party_identifier_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Party Identifier deleted successfully";
            emit self->statusMessage(QString("Party Identifier '%1' deleted").arg(code));
            emit self->partyIdentifierDeleted(code);
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
