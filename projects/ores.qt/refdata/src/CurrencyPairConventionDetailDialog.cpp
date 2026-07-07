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
#include "ores.qt/CurrencyPairConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CurrencyPairConventionDetailDialog::CurrencyPairConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyPairConventionDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CurrencyPairConventionDetailDialog::~CurrencyPairConventionDetailDialog() {
    delete ui_;
}

QTabWidget* CurrencyPairConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CurrencyPairConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CurrencyPairConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CurrencyPairConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CurrencyPairConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onCloseClicked);

    connect(ui_->pairCodeEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onCodeChanged);
    connect(ui_->pipFactorEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->tickSizeEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->advanceCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->businessDayConventionEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
}

void CurrencyPairConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CurrencyPairConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyPairConventionDetailDialog::setConvention(
    const refdata::domain::currency_pair_convention& convention) {
    convention_ = convention;
    updateUiFromConvention();
}

void CurrencyPairConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->pairCodeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->pairCodeEdit->setReadOnly(true);
    ui_->pipFactorEdit->setReadOnly(readOnly);
    ui_->tickSizeEdit->setReadOnly(readOnly);
    ui_->advanceCalendarEdit->setReadOnly(readOnly);
    ui_->businessDayConventionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CurrencyPairConventionDetailDialog::updateUiFromConvention() {
    ui_->pairCodeEdit->setText(QString::fromStdString(convention_.pair_code));
    ui_->pipFactorEdit->setText(QString::number(convention_.pip_factor));
    ui_->tickSizeEdit->setText(QString::number(convention_.tick_size));
    ui_->decimalPlacesSpinBox->setValue(convention_.decimal_places);
    ui_->advanceCalendarEdit->setText(convention_.advance_calendar ?
                                          QString::fromStdString(*convention_.advance_calendar) :
                                          QString{});
    ui_->businessDayConventionEdit->setText(
        convention_.business_day_convention ?
            QString::fromStdString(*convention_.business_day_convention) :
            QString{});
    ui_->spotRelativeCheckBox->setCheckState(
        convention_.spot_relative ? (*convention_.spot_relative ? Qt::Checked : Qt::Unchecked) :
                                    Qt::PartiallyChecked);
    ui_->endOfMonthCheckBox->setCheckState(
        convention_.end_of_month ? (*convention_.end_of_month ? Qt::Checked : Qt::Unchecked) :
                                   Qt::PartiallyChecked);

    populateProvenance(convention_.version,
                       convention_.modified_by,
                       convention_.performed_by,
                       convention_.recorded_at,
                       convention_.change_reason_code,
                       convention_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        convention_.pair_code = ui_->pairCodeEdit->text().trimmed().toStdString();
    }
    convention_.pip_factor = ui_->pipFactorEdit->text().trimmed().toDouble();
    convention_.tick_size = ui_->tickSizeEdit->text().trimmed().toDouble();
    convention_.decimal_places = ui_->decimalPlacesSpinBox->value();
    {
        const auto advance_calendar_str = ui_->advanceCalendarEdit->text().trimmed().toStdString();
        convention_.advance_calendar = advance_calendar_str.empty() ?
                                           std::nullopt :
                                           std::optional<std::string>(advance_calendar_str);
    }
    {
        const auto business_day_convention_str =
            ui_->businessDayConventionEdit->text().trimmed().toStdString();
        convention_.business_day_convention =
            business_day_convention_str.empty() ?
                std::nullopt :
                std::optional<std::string>(business_day_convention_str);
    }
    switch (ui_->spotRelativeCheckBox->checkState()) {
        case Qt::Checked:
            convention_.spot_relative = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            convention_.spot_relative = std::optional<bool>(false);
            break;
        default:
            convention_.spot_relative = std::nullopt;
            break;
    }
    switch (ui_->endOfMonthCheckBox->checkState()) {
        case Qt::Checked:
            convention_.end_of_month = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            convention_.end_of_month = std::optional<bool>(false);
            break;
        default:
            convention_.end_of_month = std::nullopt;
            break;
    }
    convention_.modified_by = username_;
}

void CurrencyPairConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CurrencyPairConventionDetailDialog::validateInput() {
    const QString pair_code_val = ui_->pairCodeEdit->text().trimmed();
    const QString pip_factor_val = ui_->pipFactorEdit->text().trimmed();
    const QString tick_size_val = ui_->tickSizeEdit->text().trimmed();

    return true && !pair_code_val.isEmpty() && !pip_factor_val.isEmpty() &&
           !tick_size_val.isEmpty();
}

void CurrencyPairConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save currency pair convention while disconnected from server.");
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
    convention_.change_reason_code = crSel->reason_code;
    convention_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving currency pair convention: " << convention_.pair_code;

    QPointer<CurrencyPairConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, convention = convention_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_currency_pair_convention_request request;
        request.data = convention;
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
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Convention saved successfully";
            QString code = QString::fromStdString(self->convention_.pair_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->conventionSaved(code);
            self->notifySaveSuccess(tr("Currency Pair Convention '%1' saved").arg(code));
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

void CurrencyPairConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete currency pair convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(convention_.pair_code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Currency Pair Convention",
        QString("Are you sure you want to delete currency pair convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting currency pair convention: " << convention_.pair_code;

    QPointer<CurrencyPairConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = convention_.pair_code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_currency_pair_convention_request request;
        request.pair_codes = {code};
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
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Convention deleted successfully";
            emit self->statusMessage(QString("Currency Pair Convention '%1' deleted").arg(code));
            emit self->conventionDeleted(code);
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
