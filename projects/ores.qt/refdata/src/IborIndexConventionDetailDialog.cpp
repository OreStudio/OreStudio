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
#include "ores.qt/IborIndexConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/ibor_index_convention_protocol.hpp"
#include "ui_IborIndexConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

IborIndexConventionDetailDialog::IborIndexConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::IborIndexConventionDetailDialog)
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

IborIndexConventionDetailDialog::~IborIndexConventionDetailDialog() {
    delete ui_;
}

QTabWidget* IborIndexConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* IborIndexConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* IborIndexConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString IborIndexConventionDetailDialog::code() const {
    return QString::fromStdString(ic_.id);
}

void IborIndexConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void IborIndexConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &IborIndexConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &IborIndexConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &IborIndexConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit,
            &QLineEdit::textChanged,
            this,
            &IborIndexConventionDetailDialog::onCodeChanged);
    connect(ui_->fixingCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &IborIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->dayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &IborIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->settlementDaysEdit,
            &QSpinBox::valueChanged,
            this,
            &IborIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->businessDayConventionEdit,
            &QLineEdit::textChanged,
            this,
            &IborIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->endOfMonthEdit,
            &QCheckBox::toggled,
            this,
            &IborIndexConventionDetailDialog::onFieldChanged);
}

void IborIndexConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void IborIndexConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void IborIndexConventionDetailDialog::setConvention(
    const refdata::domain::ibor_index_convention& ic) {
    ic_ = ic;
    updateUiFromConvention();
}

void IborIndexConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void IborIndexConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IborIndexConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->fixingCalendarEdit->setReadOnly(readOnly);
    ui_->dayCountFractionEdit->setReadOnly(readOnly);
    ui_->businessDayConventionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void IborIndexConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(ic_.id));
    ui_->fixingCalendarEdit->setText(QString::fromStdString(ic_.fixing_calendar));
    ui_->dayCountFractionEdit->setText(QString::fromStdString(ic_.day_count_fraction));
    ui_->settlementDaysEdit->setValue(ic_.settlement_days);
    ui_->businessDayConventionEdit->setText(QString::fromStdString(ic_.business_day_convention));
    ui_->endOfMonthEdit->setChecked(ic_.end_of_month);

    populateProvenance(ic_.version,
                       ic_.modified_by,
                       ic_.performed_by,
                       ic_.recorded_at,
                       ic_.change_reason_code,
                       ic_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void IborIndexConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        ic_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    ic_.fixing_calendar = ui_->fixingCalendarEdit->text().trimmed().toStdString();
    ic_.day_count_fraction = ui_->dayCountFractionEdit->text().trimmed().toStdString();
    ic_.settlement_days = ui_->settlementDaysEdit->value();
    ic_.business_day_convention = ui_->businessDayConventionEdit->text().trimmed().toStdString();
    ic_.end_of_month = ui_->endOfMonthEdit->isChecked();
    ic_.modified_by = username_;
}

void IborIndexConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IborIndexConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IborIndexConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool IborIndexConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString fixing_calendar_val = ui_->fixingCalendarEdit->text().trimmed();
    const QString day_count_fraction_val = ui_->dayCountFractionEdit->text().trimmed();
    const QString business_day_convention_val = ui_->businessDayConventionEdit->text().trimmed();

    return true && !id_val.isEmpty() && !fixing_calendar_val.isEmpty() &&
           !day_count_fraction_val.isEmpty() && !business_day_convention_val.isEmpty();
}

void IborIndexConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save IBOR index convention while disconnected from server.");
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
    ic_.change_reason_code = crSel->reason_code;
    ic_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving IBOR index convention: " << ic_.id;

    QPointer<IborIndexConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, ic = ic_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_ibor_index_convention_request request;
        request.data = ic;
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
                    BOOST_LOG_SEV(lg(), info) << "IBOR Index Convention saved successfully";
                    QString code = QString::fromStdString(self->ic_.id);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->icSaved(code);
                    self->notifySaveSuccess(tr("IBOR Index Convention '%1' saved").arg(code));
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

void IborIndexConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete IBOR index convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(ic_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete IBOR Index Convention",
        QString("Are you sure you want to delete IBOR index convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting IBOR index convention: " << ic_.id;

    QPointer<IborIndexConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = ic_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_ibor_index_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "IBOR Index Convention deleted successfully";
            emit self->statusMessage(QString("IBOR Index Convention '%1' deleted").arg(code));
            emit self->icDeleted(code);
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
