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
#include "ores.qt/TenorScheduleDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/tenor_schedule_protocol.hpp"
#include "ui_TenorScheduleDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

TenorScheduleDetailDialog::TenorScheduleDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::TenorScheduleDetailDialog)
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

TenorScheduleDetailDialog::~TenorScheduleDetailDialog() {
    delete ui_;
}

QTabWidget* TenorScheduleDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* TenorScheduleDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* TenorScheduleDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString TenorScheduleDetailDialog::code() const {
    return QString::fromStdString(schedule_.code);
}

void TenorScheduleDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenorScheduleDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &TenorScheduleDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &TenorScheduleDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &TenorScheduleDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &TenorScheduleDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &TenorScheduleDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &TenorScheduleDetailDialog::onFieldChanged);
    connect(ui_->scheduleSourceEdit,
            &QLineEdit::textChanged,
            this,
            &TenorScheduleDetailDialog::onFieldChanged);
    connect(ui_->calendarCodeEdit,
            &QLineEdit::textChanged,
            this,
            &TenorScheduleDetailDialog::onFieldChanged);
    connect(ui_->diaryEntryTypeEdit,
            &QLineEdit::textChanged,
            this,
            &TenorScheduleDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &TenorScheduleDetailDialog::onFieldChanged);
}

void TenorScheduleDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void TenorScheduleDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenorScheduleDetailDialog::setSchedule(const refdata::domain::tenor_schedule& schedule) {
    schedule_ = schedule;
    updateUiFromSchedule();
}

void TenorScheduleDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorScheduleDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorScheduleDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->scheduleSourceEdit->setReadOnly(readOnly);
    ui_->calendarCodeEdit->setReadOnly(readOnly);
    ui_->diaryEntryTypeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenorScheduleDetailDialog::updateUiFromSchedule() {
    ui_->codeEdit->setText(QString::fromStdString(schedule_.code));
    ui_->nameEdit->setText(QString::fromStdString(schedule_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(schedule_.description));
    ui_->scheduleSourceEdit->setText(QString::fromStdString(schedule_.schedule_source));
    ui_->calendarCodeEdit->setText(
        schedule_.calendar_code ? QString::fromStdString(*schedule_.calendar_code) : QString{});
    ui_->diaryEntryTypeEdit->setText(schedule_.diary_entry_type ?
                                         QString::fromStdString(*schedule_.diary_entry_type) :
                                         QString{});
    ui_->displayOrderEdit->setValue(schedule_.display_order);

    populateProvenance(schedule_.version,
                       schedule_.modified_by,
                       schedule_.performed_by,
                       schedule_.recorded_at,
                       schedule_.change_reason_code,
                       schedule_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorScheduleDetailDialog::updateScheduleFromUi() {
    if (createMode_) {
        schedule_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    schedule_.name = ui_->nameEdit->text().trimmed().toStdString();
    schedule_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    schedule_.schedule_source = ui_->scheduleSourceEdit->text().trimmed().toStdString();
    {
        const auto calendar_code_str = ui_->calendarCodeEdit->text().trimmed().toStdString();
        schedule_.calendar_code = calendar_code_str.empty() ?
                                      std::nullopt :
                                      std::optional<std::string>(calendar_code_str);
    }
    {
        const auto diary_entry_type_str = ui_->diaryEntryTypeEdit->text().trimmed().toStdString();
        schedule_.diary_entry_type = diary_entry_type_str.empty() ?
                                         std::nullopt :
                                         std::optional<std::string>(diary_entry_type_str);
    }
    schedule_.display_order = ui_->displayOrderEdit->value();
    schedule_.modified_by = username_;
}

void TenorScheduleDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorScheduleDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorScheduleDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenorScheduleDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString schedule_source_val = ui_->scheduleSourceEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty() && !schedule_source_val.isEmpty();
}

void TenorScheduleDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save tenor schedule while disconnected from server.");
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
    schedule_.change_reason_code = crSel->reason_code;
    schedule_.change_commentary = crSel->commentary;

    updateScheduleFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenor schedule: " << schedule_.code;

    QPointer<TenorScheduleDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, schedule = schedule_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_tenor_schedule_request request;
        request.data = schedule;
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
                    BOOST_LOG_SEV(lg(), info) << "Tenor Schedule saved successfully";
                    QString code = QString::fromStdString(self->schedule_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->scheduleSaved(code);
                    self->notifySaveSuccess(tr("Tenor Schedule '%1' saved").arg(code));
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

void TenorScheduleDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete tenor schedule while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(schedule_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Tenor Schedule",
        QString("Are you sure you want to delete tenor schedule '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting tenor schedule: " << schedule_.code;

    QPointer<TenorScheduleDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = schedule_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_tenor_schedule_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Tenor Schedule deleted successfully";
            emit self->statusMessage(QString("Tenor Schedule '%1' deleted").arg(code));
            emit self->scheduleDeleted(code);
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
