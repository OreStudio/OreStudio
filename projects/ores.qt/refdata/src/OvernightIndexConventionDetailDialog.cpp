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
#include "ores.qt/OvernightIndexConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/overnight_index_convention_protocol.hpp"
#include "ui_OvernightIndexConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

OvernightIndexConventionDetailDialog::OvernightIndexConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::OvernightIndexConventionDetailDialog)
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

OvernightIndexConventionDetailDialog::~OvernightIndexConventionDetailDialog() {
    delete ui_;
}

QTabWidget* OvernightIndexConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* OvernightIndexConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* OvernightIndexConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString OvernightIndexConventionDetailDialog::code() const {
    return QString::fromStdString(ni_.id);
}

void OvernightIndexConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void OvernightIndexConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &OvernightIndexConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &OvernightIndexConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &OvernightIndexConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit,
            &QLineEdit::textChanged,
            this,
            &OvernightIndexConventionDetailDialog::onCodeChanged);
    connect(ui_->fixingCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &OvernightIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->dayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &OvernightIndexConventionDetailDialog::onFieldChanged);
    connect(ui_->settlementDaysEdit,
            &QSpinBox::valueChanged,
            this,
            &OvernightIndexConventionDetailDialog::onFieldChanged);
}

void OvernightIndexConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void OvernightIndexConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void OvernightIndexConventionDetailDialog::setConvention(
    const refdata::domain::overnight_index_convention& ni) {
    ni_ = ni;
    updateUiFromConvention();
}

void OvernightIndexConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void OvernightIndexConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OvernightIndexConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->fixingCalendarEdit->setReadOnly(readOnly);
    ui_->dayCountFractionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void OvernightIndexConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(ni_.id));
    ui_->fixingCalendarEdit->setText(QString::fromStdString(ni_.fixing_calendar));
    ui_->dayCountFractionEdit->setText(QString::fromStdString(ni_.day_count_fraction));
    ui_->settlementDaysEdit->setValue(ni_.settlement_days);

    populateProvenance(ni_.version,
                       ni_.modified_by,
                       ni_.performed_by,
                       ni_.recorded_at,
                       ni_.change_reason_code,
                       ni_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void OvernightIndexConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        ni_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    ni_.fixing_calendar = ui_->fixingCalendarEdit->text().trimmed().toStdString();
    ni_.day_count_fraction = ui_->dayCountFractionEdit->text().trimmed().toStdString();
    ni_.settlement_days = ui_->settlementDaysEdit->value();
    ni_.modified_by = username_;
}

void OvernightIndexConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OvernightIndexConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OvernightIndexConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool OvernightIndexConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString fixing_calendar_val = ui_->fixingCalendarEdit->text().trimmed();
    const QString day_count_fraction_val = ui_->dayCountFractionEdit->text().trimmed();

    return true && !id_val.isEmpty() && !fixing_calendar_val.isEmpty() &&
           !day_count_fraction_val.isEmpty();
}

void OvernightIndexConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save overnight index convention while disconnected from server.");
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
    ni_.change_reason_code = crSel->reason_code;
    ni_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving overnight index convention: " << ni_.id;

    QPointer<OvernightIndexConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, ni = ni_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_overnight_index_convention_request request;
        request.data = ni;
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
                    BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention saved successfully";
                    QString code = QString::fromStdString(self->ni_.id);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->niSaved(code);
                    self->notifySaveSuccess(tr("Overnight Index Convention '%1' saved").arg(code));
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

void OvernightIndexConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete overnight index convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(ni_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Overnight Index Convention",
        QString("Are you sure you want to delete overnight index convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting overnight index convention: " << ni_.id;

    QPointer<OvernightIndexConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = ni_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_overnight_index_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Overnight Index Convention deleted successfully";
            emit self->statusMessage(QString("Overnight Index Convention '%1' deleted").arg(code));
            emit self->niDeleted(code);
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
