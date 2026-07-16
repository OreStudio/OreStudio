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
#include "ores.qt/PaymentFrequencyDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ui_PaymentFrequencyDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

PaymentFrequencyDetailDialog::PaymentFrequencyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PaymentFrequencyDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

PaymentFrequencyDetailDialog::~PaymentFrequencyDetailDialog() {
    delete ui_;
}

QTabWidget* PaymentFrequencyDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PaymentFrequencyDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PaymentFrequencyDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PaymentFrequencyDetailDialog::code() const {
    return QString::fromStdString(payment_frequency__.code);
}

void PaymentFrequencyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PaymentFrequencyDetailDialog::setupCombos() {}

void PaymentFrequencyDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &PaymentFrequencyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &PaymentFrequencyDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &PaymentFrequencyDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &PaymentFrequencyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &PaymentFrequencyDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &PaymentFrequencyDetailDialog::onFieldChanged);
    connect(ui_->periodUnitCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PaymentFrequencyDetailDialog::onFieldChanged);
}

void PaymentFrequencyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populatePeriodUnitCombo();
    setup_badge_combo(this, ui_->periodUnitCombo, badgeCache(), "tenor_unit");
}

void PaymentFrequencyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PaymentFrequencyDetailDialog::setFrequency(
    const refdata::domain::payment_frequency& payment_frequency_) {
    payment_frequency__ = payment_frequency_;
    updateUiFromFrequency();
}

void PaymentFrequencyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void PaymentFrequencyDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PaymentFrequencyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->periodUnitCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PaymentFrequencyDetailDialog::populatePeriodUnitCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating period_unit combo";
    populateDynamicCombo<refdata::domain::tenor_unit>(
        ui_->periodUnitCombo,
        this,
        clientManager_,
        &fetch_tenor_units,
        "periodUnitWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(payment_frequency__.period_unit); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load tenor units: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->periodUnitCombo, badgeCache(), "tenor_unit"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void PaymentFrequencyDetailDialog::updateUiFromFrequency() {
    ui_->codeEdit->setText(QString::fromStdString(payment_frequency__.code));
    ui_->nameEdit->setText(QString::fromStdString(payment_frequency__.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(payment_frequency__.description));
    {
        const auto val = QString::fromStdString(payment_frequency__.period_unit);
        const int idx = ui_->periodUnitCombo->findData(val);
        if (idx >= 0)
            ui_->periodUnitCombo->setCurrentIndex(idx);
    }
    ui_->periodMultiplierEdit->setValue(
        payment_frequency__.period_multiplier.value_or(ui_->periodMultiplierEdit->minimum()));
    ui_->displayOrderEdit->setValue(payment_frequency__.display_order);

    populateProvenance(payment_frequency__.version,
                       payment_frequency__.modified_by,
                       payment_frequency__.performed_by,
                       payment_frequency__.recorded_at,
                       payment_frequency__.change_reason_code,
                       payment_frequency__.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PaymentFrequencyDetailDialog::updateFrequencyFromUi() {
    if (createMode_) {
        payment_frequency__.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    payment_frequency__.name = ui_->nameEdit->text().trimmed().toStdString();
    payment_frequency__.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    payment_frequency__.period_unit = ui_->periodUnitCombo->currentText().toStdString();
    if (ui_->periodMultiplierEdit->value() == ui_->periodMultiplierEdit->minimum())
        payment_frequency__.period_multiplier = std::nullopt;
    else
        payment_frequency__.period_multiplier = ui_->periodMultiplierEdit->value();
    payment_frequency__.display_order = ui_->displayOrderEdit->value();
    payment_frequency__.modified_by = username_;
}

void PaymentFrequencyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PaymentFrequencyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PaymentFrequencyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PaymentFrequencyDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const bool period_unit_selected = ui_->periodUnitCombo->currentIndex() >= 0;

    return true && !code_val.isEmpty() && !name_val.isEmpty() && period_unit_selected;
}

void PaymentFrequencyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save payment frequency while disconnected from server.");
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
    payment_frequency__.change_reason_code = crSel->reason_code;
    payment_frequency__.change_commentary = crSel->commentary;

    updateFrequencyFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving payment frequency: " << payment_frequency__.code;

    QPointer<PaymentFrequencyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, payment_frequency_ = payment_frequency__]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_payment_frequency_request request;
        request.data = payment_frequency_;
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
            BOOST_LOG_SEV(lg(), info) << "Payment Frequency saved successfully";
            QString code = QString::fromStdString(self->payment_frequency__.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->payment_frequency_Saved(code);
            self->notifySaveSuccess(tr("Payment Frequency '%1' saved").arg(code));
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

void PaymentFrequencyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete payment frequency while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(payment_frequency__.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Payment Frequency",
        QString("Are you sure you want to delete payment frequency '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting payment frequency: " << payment_frequency__.code;

    QPointer<PaymentFrequencyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = payment_frequency__.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_payment_frequency_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Payment Frequency deleted successfully";
            emit self->statusMessage(QString("Payment Frequency '%1' deleted").arg(code));
            emit self->payment_frequency_Deleted(code);
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
