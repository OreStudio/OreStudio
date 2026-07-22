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
#include "ores.qt/CdsConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/cds_convention_protocol.hpp"
#include "ui_CdsConventionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CdsConventionDetailDialog::CdsConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CdsConventionDetailDialog)
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

CdsConventionDetailDialog::~CdsConventionDetailDialog() {
    delete ui_;
}

QTabWidget* CdsConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CdsConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CdsConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CdsConventionDetailDialog::code() const {
    return QString::fromStdString(cc_.id);
}

void CdsConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CdsConventionDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &CdsConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CdsConventionDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &CdsConventionDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &CdsConventionDetailDialog::onCodeChanged);
    connect(ui_->calendarEdit,
            &QLineEdit::textChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->frequencyEdit,
            &QLineEdit::textChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->paymentConventionEdit,
            &QLineEdit::textChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(
        ui_->ruleEdit, &QLineEdit::textChanged, this, &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->dayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->settlementDaysEdit,
            &QSpinBox::valueChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->upfrontSettlementDaysEdit,
            &QSpinBox::valueChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->lastPeriodDayCountFractionEdit,
            &QLineEdit::textChanged,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->settlesAccrualEdit,
            &QCheckBox::toggled,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
    connect(ui_->paysAtDefaultTimeEdit,
            &QCheckBox::toggled,
            this,
            &CdsConventionDetailDialog::onFieldChanged);
}

void CdsConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CdsConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CdsConventionDetailDialog::setConvention(const refdata::domain::cds_convention& cc) {
    cc_ = cc;
    updateUiFromConvention();
}

void CdsConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CdsConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CdsConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarEdit->setReadOnly(readOnly);
    ui_->frequencyEdit->setReadOnly(readOnly);
    ui_->paymentConventionEdit->setReadOnly(readOnly);
    ui_->ruleEdit->setReadOnly(readOnly);
    ui_->dayCountFractionEdit->setReadOnly(readOnly);
    ui_->lastPeriodDayCountFractionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CdsConventionDetailDialog::updateUiFromConvention() {
    ui_->idEdit->setText(QString::fromStdString(cc_.id));
    ui_->calendarEdit->setText(QString::fromStdString(cc_.calendar));
    ui_->frequencyEdit->setText(QString::fromStdString(cc_.frequency));
    ui_->paymentConventionEdit->setText(QString::fromStdString(cc_.payment_convention));
    ui_->ruleEdit->setText(QString::fromStdString(cc_.rule));
    ui_->dayCountFractionEdit->setText(QString::fromStdString(cc_.day_count_fraction));
    ui_->settlementDaysEdit->setValue(cc_.settlement_days);
    ui_->upfrontSettlementDaysEdit->setValue(
        cc_.upfront_settlement_days.value_or(ui_->upfrontSettlementDaysEdit->minimum()));
    ui_->lastPeriodDayCountFractionEdit->setText(
        cc_.last_period_day_count_fraction ?
            QString::fromStdString(*cc_.last_period_day_count_fraction) :
            QString{});
    ui_->settlesAccrualEdit->setChecked(cc_.settles_accrual);
    ui_->paysAtDefaultTimeEdit->setChecked(cc_.pays_at_default_time);

    populateProvenance(cc_.version,
                       cc_.modified_by,
                       cc_.performed_by,
                       cc_.recorded_at,
                       cc_.change_reason_code,
                       cc_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CdsConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        cc_.id = ui_->idEdit->text().trimmed().toStdString();
    }
    cc_.calendar = ui_->calendarEdit->text().trimmed().toStdString();
    cc_.frequency = ui_->frequencyEdit->text().trimmed().toStdString();
    cc_.payment_convention = ui_->paymentConventionEdit->text().trimmed().toStdString();
    cc_.rule = ui_->ruleEdit->text().trimmed().toStdString();
    cc_.day_count_fraction = ui_->dayCountFractionEdit->text().trimmed().toStdString();
    cc_.settlement_days = ui_->settlementDaysEdit->value();
    if (ui_->upfrontSettlementDaysEdit->value() == ui_->upfrontSettlementDaysEdit->minimum())
        cc_.upfront_settlement_days = std::nullopt;
    else
        cc_.upfront_settlement_days = ui_->upfrontSettlementDaysEdit->value();
    {
        const auto last_period_day_count_fraction_str =
            ui_->lastPeriodDayCountFractionEdit->text().trimmed().toStdString();
        cc_.last_period_day_count_fraction =
            last_period_day_count_fraction_str.empty() ?
                std::nullopt :
                std::optional<std::string>(last_period_day_count_fraction_str);
    }
    cc_.settles_accrual = ui_->settlesAccrualEdit->isChecked();
    cc_.pays_at_default_time = ui_->paysAtDefaultTimeEdit->isChecked();
    cc_.modified_by = username_;
}

void CdsConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CdsConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CdsConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CdsConventionDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString calendar_val = ui_->calendarEdit->text().trimmed();
    const QString frequency_val = ui_->frequencyEdit->text().trimmed();
    const QString payment_convention_val = ui_->paymentConventionEdit->text().trimmed();
    const QString rule_val = ui_->ruleEdit->text().trimmed();
    const QString day_count_fraction_val = ui_->dayCountFractionEdit->text().trimmed();

    return true && !id_val.isEmpty() && !calendar_val.isEmpty() && !frequency_val.isEmpty() &&
           !payment_convention_val.isEmpty() && !rule_val.isEmpty() &&
           !day_count_fraction_val.isEmpty();
}

void CdsConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save CDS convention while disconnected from server.");
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
    cc_.change_reason_code = crSel->reason_code;
    cc_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving CDS convention: " << cc_.id;

    QPointer<CdsConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, cc = cc_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_cds_convention_request request;
        request.data = cc;
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
                    BOOST_LOG_SEV(lg(), info) << "CDS Convention saved successfully";
                    QString code = QString::fromStdString(self->cc_.id);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->ccSaved(code);
                    self->notifySaveSuccess(tr("CDS Convention '%1' saved").arg(code));
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

void CdsConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete CDS convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(cc_.id);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete CDS Convention",
        QString("Are you sure you want to delete CDS convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting CDS convention: " << cc_.id;

    QPointer<CdsConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = cc_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_cds_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "CDS Convention deleted successfully";
            emit self->statusMessage(QString("CDS Convention '%1' deleted").arg(code));
            emit self->ccDeleted(code);
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
