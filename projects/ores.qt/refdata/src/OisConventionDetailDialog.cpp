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
#include "ores.qt/OisConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/ois_convention_protocol.hpp"
#include "ui_OisConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

OisConventionDetailDialog::OisConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::OisConventionDetailDialog)
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

OisConventionDetailDialog::~OisConventionDetailDialog() {
    delete ui_;
}

QTabWidget* OisConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* OisConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* OisConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString OisConventionDetailDialog::code() const {
    return QString::fromStdString(oc_.id);
}

void OisConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void OisConventionDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &OisConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &OisConventionDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &OisConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &OisConventionDetailDialog::onCodeChanged);
    connect(
        ui_->indexEdit, &QLineEdit::textChanged, this, &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->spotLagEdit,
            &QSpinBox::valueChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedDayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->paymentLagEdit,
            &QSpinBox::valueChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedFrequencyEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedConventionEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->fixedPaymentConventionEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(
        ui_->ruleEdit, &QLineEdit::textChanged, this, &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->paymentCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(ui_->rateCutoffEdit,
            &QSpinBox::valueChanged,
            this,
            &OisConventionDetailDialog::onFieldChanged);
    connect(
        ui_->endOfMonthEdit, &QCheckBox::toggled, this, &OisConventionDetailDialog::onFieldChanged);
}

void OisConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void OisConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void OisConventionDetailDialog::setConvention(const refdata::domain::ois_convention& oc) {
    oc_ = oc;
    updateUiFromConvention();
}

void OisConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void OisConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OisConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->indexEdit->setReadOnly(readOnly);
    ui_->fixedDayCountFractionEdit->setReadOnly(readOnly);
    ui_->fixedCalendarEdit->setReadOnly(readOnly);
    ui_->fixedFrequencyEdit->setReadOnly(readOnly);
    ui_->fixedConventionEdit->setReadOnly(readOnly);
    ui_->fixedPaymentConventionEdit->setReadOnly(readOnly);
    ui_->ruleEdit->setReadOnly(readOnly);
    ui_->paymentCalendarEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void OisConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(oc_.id));
    ui_->indexEdit->setText(QString::fromStdString(oc_.index));
    ui_->spotLagEdit->setValue(oc_.spot_lag);
    ui_->fixedDayCountFractionEdit->setText(QString::fromStdString(oc_.fixed_day_count_fraction));
    ui_->fixedCalendarEdit->setText(
        oc_.fixed_calendar ? QString::fromStdString(*oc_.fixed_calendar) : QString{});
    ui_->paymentLagEdit->setValue(oc_.payment_lag.value_or(ui_->paymentLagEdit->minimum()));
    ui_->fixedFrequencyEdit->setText(
        oc_.fixed_frequency ? QString::fromStdString(*oc_.fixed_frequency) : QString{});
    ui_->fixedConventionEdit->setText(
        oc_.fixed_convention ? QString::fromStdString(*oc_.fixed_convention) : QString{});
    ui_->fixedPaymentConventionEdit->setText(
        oc_.fixed_payment_convention ? QString::fromStdString(*oc_.fixed_payment_convention) :
                                       QString{});
    ui_->ruleEdit->setText(oc_.rule ? QString::fromStdString(*oc_.rule) : QString{});
    ui_->paymentCalendarEdit->setText(
        oc_.payment_calendar ? QString::fromStdString(*oc_.payment_calendar) : QString{});
    ui_->rateCutoffEdit->setValue(oc_.rate_cutoff.value_or(ui_->rateCutoffEdit->minimum()));
    ui_->endOfMonthEdit->setCheckState(oc_.end_of_month ?
                                           (*oc_.end_of_month ? Qt::Checked : Qt::Unchecked) :
                                           Qt::PartiallyChecked);

    populateProvenance(oc_.version,
                       oc_.modified_by,
                       oc_.performed_by,
                       oc_.recorded_at,
                       oc_.change_reason_code,
                       oc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void OisConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        oc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    oc_.index = ui_->indexEdit->text().trimmed().toStdString();
    oc_.spot_lag = ui_->spotLagEdit->value();
    oc_.fixed_day_count_fraction = ui_->fixedDayCountFractionEdit->text().trimmed().toStdString();
    {
        const auto fixed_calendar_str = ui_->fixedCalendarEdit->text().trimmed().toStdString();
        oc_.fixed_calendar = fixed_calendar_str.empty() ?
                                 std::nullopt :
                                 std::optional<std::string>(fixed_calendar_str);
    }
    if (ui_->paymentLagEdit->value() == ui_->paymentLagEdit->minimum())
        oc_.payment_lag = std::nullopt;
    else
        oc_.payment_lag = ui_->paymentLagEdit->value();
    {
        const auto fixed_frequency_str = ui_->fixedFrequencyEdit->text().trimmed().toStdString();
        oc_.fixed_frequency = fixed_frequency_str.empty() ?
                                  std::nullopt :
                                  std::optional<std::string>(fixed_frequency_str);
    }
    {
        const auto fixed_convention_str = ui_->fixedConventionEdit->text().trimmed().toStdString();
        oc_.fixed_convention = fixed_convention_str.empty() ?
                                   std::nullopt :
                                   std::optional<std::string>(fixed_convention_str);
    }
    {
        const auto fixed_payment_convention_str =
            ui_->fixedPaymentConventionEdit->text().trimmed().toStdString();
        oc_.fixed_payment_convention = fixed_payment_convention_str.empty() ?
                                           std::nullopt :
                                           std::optional<std::string>(fixed_payment_convention_str);
    }
    {
        const auto rule_str = ui_->ruleEdit->text().trimmed().toStdString();
        oc_.rule = rule_str.empty() ? std::nullopt : std::optional<std::string>(rule_str);
    }
    {
        const auto payment_calendar_str = ui_->paymentCalendarEdit->text().trimmed().toStdString();
        oc_.payment_calendar = payment_calendar_str.empty() ?
                                   std::nullopt :
                                   std::optional<std::string>(payment_calendar_str);
    }
    if (ui_->rateCutoffEdit->value() == ui_->rateCutoffEdit->minimum())
        oc_.rate_cutoff = std::nullopt;
    else
        oc_.rate_cutoff = ui_->rateCutoffEdit->value();
    switch (ui_->endOfMonthEdit->checkState()) {
        case Qt::Checked:
            oc_.end_of_month = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            oc_.end_of_month = std::optional<bool>(false);
            break;
        default:
            oc_.end_of_month = std::nullopt;
            break;
    }
    oc_.modified_by = username_;
}

void OisConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OisConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OisConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool OisConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString index_val = ui_->indexEdit->text().trimmed();
    const QString fixed_day_count_fraction_val = ui_->fixedDayCountFractionEdit->text().trimmed();

    return true && !id_val.isEmpty() && !index_val.isEmpty() &&
           !fixed_day_count_fraction_val.isEmpty();
}

void OisConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save OIS convention while disconnected from server.");
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
    oc_.change_reason_code = crSel->reason_code;
    oc_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving OIS convention: " << oc_.id;

    QPointer<OisConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, oc = oc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_ois_convention_request request;
        request.data = oc;
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
                    BOOST_LOG_SEV(lg(), info) << "OIS Convention saved successfully";
                    QString code = QString::fromStdString(self->oc_.id);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->ocSaved(code);
                    self->notifySaveSuccess(tr("OIS Convention '%1' saved").arg(code));
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

void OisConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete OIS convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(oc_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete OIS Convention",
        QString("Are you sure you want to delete OIS convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting OIS convention: " << oc_.id;

    QPointer<OisConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = oc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_ois_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "OIS Convention deleted successfully";
            emit self->statusMessage(QString("OIS Convention '%1' deleted").arg(code));
            emit self->ocDeleted(code);
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
