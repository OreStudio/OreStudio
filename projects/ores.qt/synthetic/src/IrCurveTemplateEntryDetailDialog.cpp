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
#include "ores.qt/IrCurveTemplateEntryDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include "ui_IrCurveTemplateEntryDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

IrCurveTemplateEntryDetailDialog::IrCurveTemplateEntryDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::IrCurveTemplateEntryDetailDialog)
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

IrCurveTemplateEntryDetailDialog::~IrCurveTemplateEntryDetailDialog() {
    delete ui_;
}

QTabWidget* IrCurveTemplateEntryDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* IrCurveTemplateEntryDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* IrCurveTemplateEntryDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString IrCurveTemplateEntryDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(ir_curve_template_entry_.id));
}

void IrCurveTemplateEntryDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void IrCurveTemplateEntryDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &IrCurveTemplateEntryDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &IrCurveTemplateEntryDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &IrCurveTemplateEntryDetailDialog::onCloseClicked);

    connect(ui_->tenorCodeEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveTemplateEntryDetailDialog::onFieldChanged);
    connect(ui_->instrumentCodeEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveTemplateEntryDetailDialog::onFieldChanged);
}

void IrCurveTemplateEntryDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void IrCurveTemplateEntryDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void IrCurveTemplateEntryDetailDialog::setEntry(
    const synthetic::domain::ir_curve_template_entry& ir_curve_template_entry) {
    ir_curve_template_entry_ = ir_curve_template_entry;
    updateUiFromEntry();
}

void IrCurveTemplateEntryDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        ir_curve_template_entry_.id = boost::uuids::random_generator()();
        if (clientManager_)
            ir_curve_template_entry_.party_id = clientManager_->currentPartyId();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveTemplateEntryDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveTemplateEntryDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tenorCodeEdit->setReadOnly(readOnly);
    ui_->instrumentCodeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void IrCurveTemplateEntryDetailDialog::updateUiFromEntry() {
    ui_->sequenceIndexEdit->setValue(ir_curve_template_entry_.sequence_index);
    ui_->tenorCodeEdit->setText(QString::fromStdString(ir_curve_template_entry_.tenor_code));
    ui_->instrumentCodeEdit->setText(
        QString::fromStdString(ir_curve_template_entry_.instrument_code));

    populateProvenance(ir_curve_template_entry_.version,
                       ir_curve_template_entry_.modified_by,
                       ir_curve_template_entry_.performed_by,
                       ir_curve_template_entry_.recorded_at,
                       ir_curve_template_entry_.change_reason_code,
                       ir_curve_template_entry_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveTemplateEntryDetailDialog::updateEntryFromUi() {
    ir_curve_template_entry_.sequence_index = ui_->sequenceIndexEdit->value();
    ir_curve_template_entry_.tenor_code = ui_->tenorCodeEdit->text().trimmed().toStdString();
    ir_curve_template_entry_.instrument_code =
        ui_->instrumentCodeEdit->text().trimmed().toStdString();
    ir_curve_template_entry_.modified_by = username_;
}


void IrCurveTemplateEntryDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveTemplateEntryDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool IrCurveTemplateEntryDetailDialog::validateInput() {
    const QString tenor_code_val = ui_->tenorCodeEdit->text().trimmed();
    const QString instrument_code_val = ui_->instrumentCodeEdit->text().trimmed();

    return true && !tenor_code_val.isEmpty() && !instrument_code_val.isEmpty();
}

void IrCurveTemplateEntryDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save IR curve template entry while disconnected from server.");
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
    ir_curve_template_entry_.change_reason_code = crSel->reason_code;
    ir_curve_template_entry_.change_commentary = crSel->commentary;

    updateEntryFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving IR curve template entry: "
                              << boost::uuids::to_string(ir_curve_template_entry_.id);

    QPointer<IrCurveTemplateEntryDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, ir_curve_template_entry = ir_curve_template_entry_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_ir_curve_template_entry_request request;
        request.data = ir_curve_template_entry;
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
            BOOST_LOG_SEV(lg(), info) << "IR Curve Template Entry saved successfully";
            QString code =
                QString::fromStdString(boost::uuids::to_string(self->ir_curve_template_entry_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->ir_curve_template_entrySaved(code);
            self->notifySaveSuccess(tr("IR Curve Template Entry '%1' saved").arg(code));
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

void IrCurveTemplateEntryDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete IR curve template entry while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(ir_curve_template_entry_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete IR Curve Template Entry",
        QString("Are you sure you want to delete IR curve template entry '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting IR curve template entry: "
                              << boost::uuids::to_string(ir_curve_template_entry_.id);

    QPointer<IrCurveTemplateEntryDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 id_str = boost::uuids::to_string(ir_curve_template_entry_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_ir_curve_template_entry_request request;
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
            BOOST_LOG_SEV(lg(), info) << "IR Curve Template Entry deleted successfully";
            emit self->statusMessage(QString("IR Curve Template Entry '%1' deleted").arg(code));
            emit self->ir_curve_template_entryDeleted(code);
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
