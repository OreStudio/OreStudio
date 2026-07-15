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
#include "ores.qt/SwapConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/swap_convention_protocol.hpp"
#include "ui_SwapConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

SwapConventionDetailDialog::SwapConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::SwapConventionDetailDialog)
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

SwapConventionDetailDialog::~SwapConventionDetailDialog() {
    delete ui_;
}

QTabWidget* SwapConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* SwapConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* SwapConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString SwapConventionDetailDialog::code() const {
    return QString::fromStdString(sc_.id);
}

void SwapConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void SwapConventionDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &SwapConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &SwapConventionDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &SwapConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &SwapConventionDetailDialog::onCodeChanged);
    connect(ui_->fixedFrequencyEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedDayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
    connect(
        ui_->indexEdit, &QLineEdit::textChanged, this, &SwapConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedConventionEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
    connect(ui_->floatFrequencyEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
    connect(ui_->subPeriodsCouponTypeEdit,
            &QLineEdit::textChanged,
            this,
            &SwapConventionDetailDialog::onFieldChanged);
}

void SwapConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void SwapConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void SwapConventionDetailDialog::setConvention(const refdata::domain::swap_convention& sc) {
    sc_ = sc;
    updateUiFromConvention();
}

void SwapConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void SwapConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SwapConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->fixedFrequencyEdit->setReadOnly(readOnly);
    ui_->fixedDayCountFractionEdit->setReadOnly(readOnly);
    ui_->indexEdit->setReadOnly(readOnly);
    ui_->fixedCalendarEdit->setReadOnly(readOnly);
    ui_->fixedConventionEdit->setReadOnly(readOnly);
    ui_->floatFrequencyEdit->setReadOnly(readOnly);
    ui_->subPeriodsCouponTypeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void SwapConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(sc_.id));
    ui_->fixedFrequencyEdit->setText(QString::fromStdString(sc_.fixed_frequency));
    ui_->fixedDayCountFractionEdit->setText(QString::fromStdString(sc_.fixed_day_count_fraction));
    ui_->indexEdit->setText(QString::fromStdString(sc_.index));
    ui_->fixedCalendarEdit->setText(
        sc_.fixed_calendar ? QString::fromStdString(*sc_.fixed_calendar) : QString{});
    ui_->fixedConventionEdit->setText(
        sc_.fixed_convention ? QString::fromStdString(*sc_.fixed_convention) : QString{});
    ui_->floatFrequencyEdit->setText(
        sc_.float_frequency ? QString::fromStdString(*sc_.float_frequency) : QString{});
    ui_->subPeriodsCouponTypeEdit->setText(
        sc_.sub_periods_coupon_type ? QString::fromStdString(*sc_.sub_periods_coupon_type) :
                                      QString{});

    populateProvenance(sc_.version,
                       sc_.modified_by,
                       sc_.performed_by,
                       sc_.recorded_at,
                       sc_.change_reason_code,
                       sc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void SwapConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        sc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    sc_.fixed_frequency = ui_->fixedFrequencyEdit->text().trimmed().toStdString();
    sc_.fixed_day_count_fraction = ui_->fixedDayCountFractionEdit->text().trimmed().toStdString();
    sc_.index = ui_->indexEdit->text().trimmed().toStdString();
    {
        const auto fixed_calendar_str = ui_->fixedCalendarEdit->text().trimmed().toStdString();
        sc_.fixed_calendar = fixed_calendar_str.empty() ?
                                 std::nullopt :
                                 std::optional<std::string>(fixed_calendar_str);
    }
    {
        const auto fixed_convention_str = ui_->fixedConventionEdit->text().trimmed().toStdString();
        sc_.fixed_convention = fixed_convention_str.empty() ?
                                   std::nullopt :
                                   std::optional<std::string>(fixed_convention_str);
    }
    {
        const auto float_frequency_str = ui_->floatFrequencyEdit->text().trimmed().toStdString();
        sc_.float_frequency = float_frequency_str.empty() ?
                                  std::nullopt :
                                  std::optional<std::string>(float_frequency_str);
    }
    {
        const auto sub_periods_coupon_type_str =
            ui_->subPeriodsCouponTypeEdit->text().trimmed().toStdString();
        sc_.sub_periods_coupon_type = sub_periods_coupon_type_str.empty() ?
                                          std::nullopt :
                                          std::optional<std::string>(sub_periods_coupon_type_str);
    }
    sc_.modified_by = username_;
}

void SwapConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SwapConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SwapConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool SwapConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString fixed_frequency_val = ui_->fixedFrequencyEdit->text().trimmed();
    const QString fixed_day_count_fraction_val = ui_->fixedDayCountFractionEdit->text().trimmed();
    const QString index_val = ui_->indexEdit->text().trimmed();

    return true && !id_val.isEmpty() && !fixed_frequency_val.isEmpty() &&
           !fixed_day_count_fraction_val.isEmpty() && !index_val.isEmpty();
}

void SwapConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save swap convention while disconnected from server.");
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
    sc_.change_reason_code = crSel->reason_code;
    sc_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving swap convention: " << sc_.id;

    QPointer<SwapConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, sc = sc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_swap_convention_request request;
        request.data = sc;
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
            BOOST_LOG_SEV(lg(), info) << "Swap Convention saved successfully";
            QString code = QString::fromStdString(self->sc_.id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->scSaved(code);
            self->notifySaveSuccess(tr("Swap Convention '%1' saved").arg(code));
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

void SwapConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete swap convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(sc_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Swap Convention",
        QString("Are you sure you want to delete swap convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting swap convention: " << sc_.id;

    QPointer<SwapConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = sc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_swap_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Swap Convention deleted successfully";
            emit self->statusMessage(QString("Swap Convention '%1' deleted").arg(code));
            emit self->scDeleted(code);
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
