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
#include "ores.qt/DepositConventionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_DepositConventionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/deposit_convention_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

DepositConventionDetailDialog::DepositConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::DepositConventionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

DepositConventionDetailDialog::~DepositConventionDetailDialog() {
    delete ui_;
}

QTabWidget* DepositConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* DepositConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* DepositConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void DepositConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void DepositConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &DepositConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &DepositConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &DepositConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this,
            &DepositConventionDetailDialog::onCodeChanged);
    connect(ui_->indexEdit, &QLineEdit::textChanged, this,
            &DepositConventionDetailDialog::onFieldChanged);
    connect(ui_->calendarEdit, &QLineEdit::textChanged, this,
            &DepositConventionDetailDialog::onFieldChanged);
    connect(ui_->conventionEdit, &QLineEdit::textChanged, this,
            &DepositConventionDetailDialog::onFieldChanged);
    connect(ui_->dayCountFractionEdit, &QLineEdit::textChanged, this,
            &DepositConventionDetailDialog::onFieldChanged);
}

void DepositConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void DepositConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void DepositConventionDetailDialog::setConvention(
    const refdata::domain::deposit_convention& dc) {
    dc_ = dc;
    updateUiFromConvention();
}

void DepositConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void DepositConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->indexEdit->setReadOnly(readOnly);
    ui_->calendarEdit->setReadOnly(readOnly);
    ui_->conventionEdit->setReadOnly(readOnly);
    ui_->dayCountFractionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void DepositConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(dc_.id));
    ui_->indexBasedEdit->setChecked(dc_.index_based);
    ui_->indexEdit->setText(dc_.index
        ? QString::fromStdString(*dc_.index)
        : QString{});
    ui_->calendarEdit->setText(dc_.calendar
        ? QString::fromStdString(*dc_.calendar)
        : QString{});
    ui_->conventionEdit->setText(dc_.convention
        ? QString::fromStdString(*dc_.convention)
        : QString{});
    ui_->dayCountFractionEdit->setText(dc_.day_count_fraction
        ? QString::fromStdString(*dc_.day_count_fraction)
        : QString{});
    ui_->endOfMonthEdit->setCheckState(dc_.end_of_month
        ? (*dc_.end_of_month ? Qt::Checked : Qt::Unchecked)
        : Qt::PartiallyChecked);
    ui_->settlementDaysEdit->setValue(dc_.settlement_days.value_or(
        ui_->settlementDaysEdit->minimum()));

    populateProvenance(dc_.version,
                       dc_.modified_by,
                       dc_.performed_by,
                       dc_.recorded_at,
                       dc_.change_reason_code,
                       dc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void DepositConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        dc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    dc_.index_based = ui_->indexBasedEdit->isChecked();
    {
        const auto index_str = ui_->indexEdit->text().trimmed().toStdString();
        dc_.index =
            index_str.empty() ? std::nullopt : std::optional<std::string>(index_str);
    }
    {
        const auto calendar_str = ui_->calendarEdit->text().trimmed().toStdString();
        dc_.calendar =
            calendar_str.empty() ? std::nullopt : std::optional<std::string>(calendar_str);
    }
    {
        const auto convention_str = ui_->conventionEdit->text().trimmed().toStdString();
        dc_.convention =
            convention_str.empty() ? std::nullopt : std::optional<std::string>(convention_str);
    }
    {
        const auto day_count_fraction_str = ui_->dayCountFractionEdit->text().trimmed().toStdString();
        dc_.day_count_fraction =
            day_count_fraction_str.empty() ? std::nullopt : std::optional<std::string>(day_count_fraction_str);
    }
    switch (ui_->endOfMonthEdit->checkState()) {
    case Qt::Checked:
        dc_.end_of_month = std::optional<bool>(true);
        break;
    case Qt::Unchecked:
        dc_.end_of_month = std::optional<bool>(false);
        break;
    default:
        dc_.end_of_month = std::nullopt;
        break;
    }
    if (ui_->settlementDaysEdit->value() == ui_->settlementDaysEdit->minimum())
        dc_.settlement_days = std::nullopt;
    else
        dc_.settlement_days = ui_->settlementDaysEdit->value();
    dc_.modified_by = username_;
}

void DepositConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DepositConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DepositConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool DepositConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();

    return true
        && !id_val.isEmpty()
    ;
}

void DepositConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save deposit convention while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving deposit convention: "
        << dc_.id;

    QPointer<DepositConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, dc = dc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_deposit_convention_request request;
        request.data = dc;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Deposit Convention saved successfully";
            QString code = QString::fromStdString(
                self->dc_.id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->dcSaved(code);
            self->notifySaveSuccess(tr("Deposit Convention '%1' saved").arg(code));
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

void DepositConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete deposit convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        dc_.id);
    auto reply = MessageBoxHelper::question(this, "Delete Deposit Convention",
        QString("Are you sure you want to delete deposit convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting deposit convention: "
        << dc_.id;

    QPointer<DepositConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = dc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_deposit_convention_request request;
        request.codes = {code};
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Deposit Convention deleted successfully";
            emit self->statusMessage(
                QString("Deposit Convention '%1' deleted").arg(code));
            emit self->dcDeleted(code);
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
