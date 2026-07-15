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
#include "ores.qt/PartyDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ui_PartyDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

PartyDetailDialog::PartyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PartyDetailDialog)
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
    hierarchyTab_ = new PartyHierarchyTab(this);
    hierarchyTab_->attachTo(tabWidget());
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
    childTables_ = new PartyChildEntityTables(this);
    childTables_->attachTo(tabWidget());
}

PartyDetailDialog::~PartyDetailDialog() {
    delete ui_;
}

QTabWidget* PartyDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PartyDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PartyDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PartyDetailDialog::code() const {
    return QString::fromStdString(party_.short_code);
}

void PartyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PartyDetailDialog::setupCombos() {}

void PartyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &PartyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &PartyDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &PartyDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &PartyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &PartyDetailDialog::onFieldChanged);
    connect(ui_->partyTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->statusCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->businessCenterCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PartyDetailDialog::onFieldChanged);
}

void PartyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populatePartyTypeCombo();
    setup_badge_combo(this, ui_->partyTypeCombo, badgeCache(), "party_type");
    populatePartyStatusCombo();
    setup_badge_combo(this, ui_->statusCombo, badgeCache(), "party_status");
    populateBusinessCenterCodeCombo();
}

void PartyDetailDialog::populateBusinessCenterCodeCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<PartyDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->businessCenterCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setParty() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = QString::fromStdString(self->party_.business_center_code);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(
                    combo, self->imageCache(), FlagSource::BusinessCentre, single_flag_icon_size());
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_business_centre_codes(cm); }));
}

void PartyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyDetailDialog::setParty(const refdata::domain::party& party) {
    party_ = party;
    updateUiFromParty();
    childTables_->reload(party_.id, clientManager_, username_, imageCache(), changeReasonCache());
    hierarchyTab_->reload(party_.id, clientManager_);
}

void PartyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        party_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->partyTypeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->businessCenterCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    childTables_->setReadOnly(readOnly);
}

void PartyDetailDialog::populatePartyTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating party_type combo";
    populateDynamicCombo<refdata::domain::party_type>(
        ui_->partyTypeCombo,
        this,
        clientManager_,
        &fetch_party_types,
        "partyTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(party_.party_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load party types: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->partyTypeCombo, badgeCache(), "party_type"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void PartyDetailDialog::populatePartyStatusCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating status combo";
    populateDynamicCombo<refdata::domain::party_status>(
        ui_->statusCombo,
        this,
        clientManager_,
        &fetch_party_statuses,
        "partyStatusWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(party_.status); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load party statuses: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->statusCombo, badgeCache(), "party_status"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void PartyDetailDialog::updateUiFromParty() {
    ui_->codeEdit->setText(QString::fromStdString(party_.short_code));
    ui_->nameEdit->setText(QString::fromStdString(party_.full_name));
    {
        const auto val = QString::fromStdString(party_.party_type);
        const int idx = ui_->partyTypeCombo->findData(val);
        if (idx >= 0)
            ui_->partyTypeCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(party_.status);
        const int idx = ui_->statusCombo->findData(val);
        if (idx >= 0)
            ui_->statusCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(party_.business_center_code);
        const int idx = ui_->businessCenterCombo->findText(val);
        ui_->businessCenterCombo->setCurrentIndex(idx);
    }

    populateProvenance(party_.version,
                       party_.modified_by,
                       party_.performed_by,
                       party_.recorded_at,
                       party_.change_reason_code,
                       party_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyDetailDialog::updatePartyFromUi() {
    if (createMode_) {
        party_.short_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    party_.full_name = ui_->nameEdit->text().trimmed().toStdString();
    party_.party_type = ui_->partyTypeCombo->currentText().toStdString();
    party_.status = ui_->statusCombo->currentText().toStdString();
    party_.business_center_code = ui_->businessCenterCombo->currentText().toStdString();
    party_.modified_by = username_;
}

void PartyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyDetailDialog::validateInput() {
    const QString short_code_val = ui_->codeEdit->text().trimmed();
    const QString full_name_val = ui_->nameEdit->text().trimmed();

    return true && !short_code_val.isEmpty() && !full_name_val.isEmpty();
}

void PartyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save party while disconnected from server.");
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
    party_.change_reason_code = crSel->reason_code;
    party_.change_commentary = crSel->commentary;

    updatePartyFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party: " << party_.short_code;

    QPointer<PartyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, party = party_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_party_request request;
        request.data = party;
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
            BOOST_LOG_SEV(lg(), info) << "Party saved successfully";
            QString code = QString::fromStdString(self->party_.short_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->partySaved(code);
            self->notifySaveSuccess(tr("Party '%1' saved").arg(code));
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

void PartyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete party while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(party_.short_code);
    auto reply =
        MessageBoxHelper::question(this,
                                   "Delete Party",
                                   QString("Are you sure you want to delete party '%1'?").arg(code),
                                   QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting party: " << party_.short_code;

    QPointer<PartyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(party_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_party_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Party deleted successfully";
            emit self->statusMessage(QString("Party '%1' deleted").arg(code));
            emit self->partyDeleted(code);
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
