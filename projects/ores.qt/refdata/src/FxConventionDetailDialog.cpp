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
#include "ores.qt/FxConventionDetailDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/fx_convention_protocol.hpp"
#include "ui_FxConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

FxConventionDetailDialog::FxConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::FxConventionDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

FxConventionDetailDialog::~FxConventionDetailDialog() {
    delete ui_;
}

QTabWidget* FxConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FxConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FxConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FxConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FxConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &FxConventionDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &FxConventionDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &FxConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &FxConventionDetailDialog::onCodeChanged);
    connect(ui_->sourceCurrencyEdit,
            &QLineEdit::textChanged,
            this,
            &FxConventionDetailDialog::onFieldChanged);
    connect(ui_->targetCurrencyEdit,
            &QLineEdit::textChanged,
            this,
            &FxConventionDetailDialog::onFieldChanged);
    connect(ui_->advanceCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &FxConventionDetailDialog::onFieldChanged);
    connect(ui_->conventionEdit,
            &QLineEdit::textChanged,
            this,
            &FxConventionDetailDialog::onFieldChanged);
}

void FxConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FxConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FxConventionDetailDialog::setConvention(const refdata::domain::fx_convention& fxc) {
    fxc_ = fxc;
    updateUiFromConvention();
}

void FxConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void FxConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->sourceCurrencyEdit->setReadOnly(readOnly);
    ui_->targetCurrencyEdit->setReadOnly(readOnly);
    ui_->advanceCalendarEdit->setReadOnly(readOnly);
    ui_->conventionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void FxConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(fxc_.id));
    ui_->sourceCurrencyEdit->setText(QString::fromStdString(fxc_.source_currency));
    ui_->targetCurrencyEdit->setText(QString::fromStdString(fxc_.target_currency));
    ui_->spotDaysEdit->setValue(fxc_.spot_days);
    ui_->advanceCalendarEdit->setText(
        fxc_.advance_calendar ? QString::fromStdString(*fxc_.advance_calendar) : QString{});
    ui_->conventionEdit->setText(fxc_.convention ? QString::fromStdString(*fxc_.convention) :
                                                   QString{});
    ui_->spotRelativeEdit->setCheckState(fxc_.spot_relative ?
                                             (*fxc_.spot_relative ? Qt::Checked : Qt::Unchecked) :
                                             Qt::PartiallyChecked);
    ui_->endOfMonthEdit->setCheckState(fxc_.end_of_month ?
                                           (*fxc_.end_of_month ? Qt::Checked : Qt::Unchecked) :
                                           Qt::PartiallyChecked);

    populateProvenance(fxc_.version,
                       fxc_.modified_by,
                       fxc_.performed_by,
                       fxc_.recorded_at,
                       fxc_.change_reason_code,
                       fxc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FxConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        fxc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    fxc_.source_currency = ui_->sourceCurrencyEdit->text().trimmed().toStdString();
    fxc_.target_currency = ui_->targetCurrencyEdit->text().trimmed().toStdString();
    fxc_.spot_days = ui_->spotDaysEdit->value();
    {
        const auto advance_calendar_str = ui_->advanceCalendarEdit->text().trimmed().toStdString();
        fxc_.advance_calendar = advance_calendar_str.empty() ?
                                    std::nullopt :
                                    std::optional<std::string>(advance_calendar_str);
    }
    {
        const auto convention_str = ui_->conventionEdit->text().trimmed().toStdString();
        fxc_.convention =
            convention_str.empty() ? std::nullopt : std::optional<std::string>(convention_str);
    }
    switch (ui_->spotRelativeEdit->checkState()) {
        case Qt::Checked:
            fxc_.spot_relative = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            fxc_.spot_relative = std::optional<bool>(false);
            break;
        default:
            fxc_.spot_relative = std::nullopt;
            break;
    }
    switch (ui_->endOfMonthEdit->checkState()) {
        case Qt::Checked:
            fxc_.end_of_month = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            fxc_.end_of_month = std::optional<bool>(false);
            break;
        default:
            fxc_.end_of_month = std::nullopt;
            break;
    }
    fxc_.modified_by = username_;
}

void FxConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool FxConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString source_currency_val = ui_->sourceCurrencyEdit->text().trimmed();
    const QString target_currency_val = ui_->targetCurrencyEdit->text().trimmed();

    return true && !id_val.isEmpty() && !source_currency_val.isEmpty() &&
           !target_currency_val.isEmpty();
}

void FxConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save FX convention while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving FX convention: " << fxc_.id;

    QPointer<FxConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, fxc = fxc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_fx_convention_request request;
        request.data = fxc;
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
            BOOST_LOG_SEV(lg(), info) << "FX Convention saved successfully";
            QString code = QString::fromStdString(self->fxc_.id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->fxcSaved(code);
            self->notifySaveSuccess(tr("FX Convention '%1' saved").arg(code));
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

void FxConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete FX convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(fxc_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete FX Convention",
        QString("Are you sure you want to delete FX convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting FX convention: " << fxc_.id;

    QPointer<FxConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = fxc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_fx_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "FX Convention deleted successfully";
            emit self->statusMessage(QString("FX Convention '%1' deleted").arg(code));
            emit self->fxcDeleted(code);
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
