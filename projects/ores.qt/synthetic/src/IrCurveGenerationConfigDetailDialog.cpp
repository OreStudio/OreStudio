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
#include "ores.qt/IrCurveGenerationConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ui_IrCurveGenerationConfigDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

IrCurveGenerationConfigDetailDialog::IrCurveGenerationConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::IrCurveGenerationConfigDetailDialog)
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

IrCurveGenerationConfigDetailDialog::~IrCurveGenerationConfigDetailDialog() {
    delete ui_;
}

QTabWidget* IrCurveGenerationConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* IrCurveGenerationConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* IrCurveGenerationConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString IrCurveGenerationConfigDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config_.id));
}

void IrCurveGenerationConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void IrCurveGenerationConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &IrCurveGenerationConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &IrCurveGenerationConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &IrCurveGenerationConfigDetailDialog::onCloseClicked);

    connect(ui_->currencyEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->indexNameEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->processTypeEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->kappaEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->thetaEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->sigmaEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->initialRateEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->fixedLegPaymentFrequencyEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->enabledCheck,
            &QCheckBox::toggled,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->autoStartCheck,
            &QCheckBox::toggled,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &IrCurveGenerationConfigDetailDialog::onFieldChanged);
}

void IrCurveGenerationConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void IrCurveGenerationConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void IrCurveGenerationConfigDetailDialog::setConfig(
    const synthetic::domain::ir_curve_generation_config& ir_curve_generation_config) {
    ir_curve_generation_config_ = ir_curve_generation_config;
    updateUiFromConfig();
}

void IrCurveGenerationConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        ir_curve_generation_config_.id = boost::uuids::random_generator()();
        if (clientManager_)
            ir_curve_generation_config_.party_id = clientManager_->currentPartyId();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveGenerationConfigDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveGenerationConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->indexNameEdit->setReadOnly(readOnly);
    ui_->processTypeEdit->setReadOnly(readOnly);
    ui_->kappaEdit->setReadOnly(readOnly);
    ui_->thetaEdit->setReadOnly(readOnly);
    ui_->sigmaEdit->setReadOnly(readOnly);
    ui_->initialRateEdit->setReadOnly(readOnly);
    ui_->fixedLegPaymentFrequencyEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void IrCurveGenerationConfigDetailDialog::updateUiFromConfig() {
    ui_->currencyEdit->setText(QString::fromStdString(ir_curve_generation_config_.currency_code));
    ui_->indexNameEdit->setText(QString::fromStdString(ir_curve_generation_config_.index_name));
    ui_->processTypeEdit->setText(QString::fromStdString(ir_curve_generation_config_.process_type));
    ui_->kappaEdit->setText(QString::number(ir_curve_generation_config_.kappa));
    ui_->thetaEdit->setText(QString::number(ir_curve_generation_config_.theta));
    ui_->sigmaEdit->setText(QString::number(ir_curve_generation_config_.sigma));
    ui_->initialRateEdit->setText(QString::number(ir_curve_generation_config_.initial_rate));
    ui_->ticksPerHourEdit->setValue(ir_curve_generation_config_.ticks_per_hour);
    ui_->fixedLegPaymentFrequencyEdit->setText(
        QString::fromStdString(ir_curve_generation_config_.fixed_leg_payment_frequency_code));
    ui_->enabledCheck->setChecked(ir_curve_generation_config_.enabled);
    ui_->autoStartCheck->setChecked(ir_curve_generation_config_.auto_start);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(ir_curve_generation_config_.description));

    populateProvenance(ir_curve_generation_config_.version,
                       ir_curve_generation_config_.modified_by,
                       ir_curve_generation_config_.performed_by,
                       ir_curve_generation_config_.recorded_at,
                       ir_curve_generation_config_.change_reason_code,
                       ir_curve_generation_config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveGenerationConfigDetailDialog::updateConfigFromUi() {
    ir_curve_generation_config_.currency_code = ui_->currencyEdit->text().trimmed().toStdString();
    ir_curve_generation_config_.index_name = ui_->indexNameEdit->text().trimmed().toStdString();
    ir_curve_generation_config_.process_type = ui_->processTypeEdit->text().trimmed().toStdString();
    ir_curve_generation_config_.kappa = ui_->kappaEdit->text().trimmed().toDouble();
    ir_curve_generation_config_.theta = ui_->thetaEdit->text().trimmed().toDouble();
    ir_curve_generation_config_.sigma = ui_->sigmaEdit->text().trimmed().toDouble();
    ir_curve_generation_config_.initial_rate = ui_->initialRateEdit->text().trimmed().toDouble();
    ir_curve_generation_config_.ticks_per_hour = ui_->ticksPerHourEdit->value();
    ir_curve_generation_config_.fixed_leg_payment_frequency_code =
        ui_->fixedLegPaymentFrequencyEdit->text().trimmed().toStdString();
    ir_curve_generation_config_.enabled = ui_->enabledCheck->isChecked();
    ir_curve_generation_config_.auto_start = ui_->autoStartCheck->isChecked();
    ir_curve_generation_config_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    ir_curve_generation_config_.modified_by = username_;
}


void IrCurveGenerationConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveGenerationConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool IrCurveGenerationConfigDetailDialog::validateInput() {
    const QString currency_code_val = ui_->currencyEdit->text().trimmed();
    const QString index_name_val = ui_->indexNameEdit->text().trimmed();
    const QString process_type_val = ui_->processTypeEdit->text().trimmed();
    const QString kappa_val = ui_->kappaEdit->text().trimmed();
    const QString theta_val = ui_->thetaEdit->text().trimmed();
    const QString sigma_val = ui_->sigmaEdit->text().trimmed();
    const QString initial_rate_val = ui_->initialRateEdit->text().trimmed();
    const QString fixed_leg_payment_frequency_code_val =
        ui_->fixedLegPaymentFrequencyEdit->text().trimmed();

    return true && !currency_code_val.isEmpty() && !index_name_val.isEmpty() &&
           !process_type_val.isEmpty() && !kappa_val.isEmpty() && !theta_val.isEmpty() &&
           !sigma_val.isEmpty() && !initial_rate_val.isEmpty() &&
           !fixed_leg_payment_frequency_code_val.isEmpty();
}

void IrCurveGenerationConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save IR curve generation config while disconnected from server.");
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
    ir_curve_generation_config_.change_reason_code = crSel->reason_code;
    ir_curve_generation_config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving IR curve generation config: "
                              << boost::uuids::to_string(ir_curve_generation_config_.id);

    QPointer<IrCurveGenerationConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, ir_curve_generation_config = ir_curve_generation_config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_ir_curve_generation_config_request request;
        request.data = ir_curve_generation_config;
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
            [self, watcher]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "IR Curve Generation Config saved successfully";
                    QString code = QString::fromStdString(
                        boost::uuids::to_string(self->ir_curve_generation_config_.id));
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->ir_curve_generation_configSaved(code);
                    self->notifySaveSuccess(tr("IR Curve Generation Config '%1' saved").arg(code));
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

void IrCurveGenerationConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete IR curve generation config while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(ir_curve_generation_config_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete IR Curve Generation Config",
        QString("Are you sure you want to delete IR curve generation config '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting IR curve generation config: "
                              << boost::uuids::to_string(ir_curve_generation_config_.id);

    QPointer<IrCurveGenerationConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task =
        [self, id_str = boost::uuids::to_string(ir_curve_generation_config_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_ir_curve_generation_config_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "IR Curve Generation Config deleted successfully";
            emit self->statusMessage(QString("IR Curve Generation Config '%1' deleted").arg(code));
            emit self->ir_curve_generation_configDeleted(code);
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
