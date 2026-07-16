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
#include "ores.qt/BusinessUnitTypeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/business_unit_type_protocol.hpp"
#include "ui_BusinessUnitTypeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

BusinessUnitTypeDetailDialog::BusinessUnitTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::BusinessUnitTypeDetailDialog)
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

BusinessUnitTypeDetailDialog::~BusinessUnitTypeDetailDialog() {
    delete ui_;
}

QTabWidget* BusinessUnitTypeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BusinessUnitTypeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BusinessUnitTypeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString BusinessUnitTypeDetailDialog::code() const {
    return QString::fromStdString(business_unit_type_.code);
}

void BusinessUnitTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BusinessUnitTypeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &BusinessUnitTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &BusinessUnitTypeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &BusinessUnitTypeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &BusinessUnitTypeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
    connect(ui_->codingSchemeCodeEdit,
            &QLineEdit::textChanged,
            this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
}

void BusinessUnitTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BusinessUnitTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessUnitTypeDetailDialog::setType(
    const refdata::domain::business_unit_type& business_unit_type) {
    business_unit_type_ = business_unit_type;
    updateUiFromType();
}

void BusinessUnitTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        business_unit_type_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->codingSchemeCodeEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessUnitTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(business_unit_type_.code));
    ui_->nameEdit->setText(QString::fromStdString(business_unit_type_.name));
    ui_->codingSchemeCodeEdit->setText(
        QString::fromStdString(business_unit_type_.coding_scheme_code));
    ui_->levelEdit->setValue(business_unit_type_.level);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(business_unit_type_.description));

    populateProvenance(business_unit_type_.version,
                       business_unit_type_.modified_by,
                       business_unit_type_.performed_by,
                       business_unit_type_.recorded_at,
                       business_unit_type_.change_reason_code,
                       business_unit_type_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        business_unit_type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    business_unit_type_.name = ui_->nameEdit->text().trimmed().toStdString();
    business_unit_type_.coding_scheme_code =
        ui_->codingSchemeCodeEdit->text().trimmed().toStdString();
    business_unit_type_.level = ui_->levelEdit->value();
    business_unit_type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    business_unit_type_.modified_by = username_;
}

void BusinessUnitTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessUnitTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString coding_scheme_code_val = ui_->codingSchemeCodeEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty() && !coding_scheme_code_val.isEmpty();
}

void BusinessUnitTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save business unit type while disconnected from server.");
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
    business_unit_type_.change_reason_code = crSel->reason_code;
    business_unit_type_.change_commentary = crSel->commentary;

    updateTypeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business unit type: " << business_unit_type_.code;

    QPointer<BusinessUnitTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, business_unit_type = business_unit_type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_unit_type_request request;
        request.data = business_unit_type;
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
            BOOST_LOG_SEV(lg(), info) << "Business Unit Type saved successfully";
            QString code = QString::fromStdString(self->business_unit_type_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->business_unit_typeSaved(code);
            self->notifySaveSuccess(tr("Business Unit Type '%1' saved").arg(code));
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

void BusinessUnitTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete business unit type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(business_unit_type_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Business Unit Type",
        QString("Are you sure you want to delete business unit type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting business unit type: " << business_unit_type_.code;

    QPointer<BusinessUnitTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(business_unit_type_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_unit_type_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Business Unit Type deleted successfully";
            emit self->statusMessage(QString("Business Unit Type '%1' deleted").arg(code));
            emit self->business_unit_typeDeleted(code);
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
