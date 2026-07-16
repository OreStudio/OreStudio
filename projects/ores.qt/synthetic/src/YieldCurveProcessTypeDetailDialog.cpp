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
#include "ores.qt/YieldCurveProcessTypeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_type_protocol.hpp"
#include "ui_YieldCurveProcessTypeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

YieldCurveProcessTypeDetailDialog::YieldCurveProcessTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::YieldCurveProcessTypeDetailDialog)
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

YieldCurveProcessTypeDetailDialog::~YieldCurveProcessTypeDetailDialog() {
    delete ui_;
}

QTabWidget* YieldCurveProcessTypeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* YieldCurveProcessTypeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* YieldCurveProcessTypeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString YieldCurveProcessTypeDetailDialog::code() const {
    return QString::fromStdString(process_type_.code);
}

void YieldCurveProcessTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void YieldCurveProcessTypeDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessTypeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessTypeDetailDialog::onCloseClicked);

    connect(ui_->codeEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessTypeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessTypeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &YieldCurveProcessTypeDetailDialog::onFieldChanged);
}

void YieldCurveProcessTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void YieldCurveProcessTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void YieldCurveProcessTypeDetailDialog::setType(
    const synthetic::domain::yield_curve_process_type& process_type) {
    process_type_ = process_type;
    updateUiFromType();
}

void YieldCurveProcessTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void YieldCurveProcessTypeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void YieldCurveProcessTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(process_type_.code));
    ui_->nameEdit->setText(QString::fromStdString(process_type_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(process_type_.description));
    ui_->displayOrderEdit->setValue(process_type_.display_order);

    populateProvenance(process_type_.version,
                       process_type_.modified_by,
                       process_type_.performed_by,
                       process_type_.recorded_at,
                       process_type_.change_reason_code,
                       process_type_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void YieldCurveProcessTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        process_type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    process_type_.name = ui_->nameEdit->text().trimmed().toStdString();
    process_type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    process_type_.display_order = ui_->displayOrderEdit->value();
    process_type_.modified_by = username_;
}

void YieldCurveProcessTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool YieldCurveProcessTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void YieldCurveProcessTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save yield curve process type while disconnected from server.");
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
    process_type_.change_reason_code = crSel->reason_code;
    process_type_.change_commentary = crSel->commentary;

    updateTypeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving yield curve process type: " << process_type_.code;

    QPointer<YieldCurveProcessTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, process_type = process_type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_yield_curve_process_type_request request;
        request.data = process_type;
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
            BOOST_LOG_SEV(lg(), info) << "Yield Curve Process Type saved successfully";
            QString code = QString::fromStdString(self->process_type_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->process_typeSaved(code);
            self->notifySaveSuccess(tr("Yield Curve Process Type '%1' saved").arg(code));
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

void YieldCurveProcessTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete yield curve process type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(process_type_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Yield Curve Process Type",
        QString("Are you sure you want to delete yield curve process type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting yield curve process type: " << process_type_.code;

    QPointer<YieldCurveProcessTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = process_type_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_yield_curve_process_type_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Yield Curve Process Type deleted successfully";
            emit self->statusMessage(QString("Yield Curve Process Type '%1' deleted").arg(code));
            emit self->process_typeDeleted(code);
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
