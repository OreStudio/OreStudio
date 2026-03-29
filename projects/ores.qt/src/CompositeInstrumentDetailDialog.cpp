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
#include "ores.qt/CompositeInstrumentDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CompositeInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CompositeInstrumentDetailDialog::CompositeInstrumentDetailDialog(
    QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CompositeInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CompositeInstrumentDetailDialog::~CompositeInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* CompositeInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CompositeInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CompositeInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CompositeInstrumentDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);
    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor));
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(
            Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CompositeInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CompositeInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CompositeInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CompositeInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &CompositeInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &CompositeInstrumentDetailDialog::onFieldChanged);
    connect(ui_->legsWidget, &CompositeLegsWidget::legsChanged, this,
            &CompositeInstrumentDetailDialog::onFieldChanged);
}

void CompositeInstrumentDetailDialog::setClientManager(
    ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CompositeInstrumentDetailDialog::setUsername(
    const std::string& username) {
    username_ = username;
}

void CompositeInstrumentDetailDialog::setCompositeInstrument(
    const trading::domain::composite_instrument& v) {
    instrument_ = v;
    updateUiFromCompositeInstrument();
    if (!instrument_.id.is_nil())
        loadLegs();
}

void CompositeInstrumentDetailDialog::setLegs(
    const std::vector<trading::domain::composite_leg>& legs) {
    // Suppress hasChanges_ side-effect while restoring server data.
    const bool saved = hasChanges_;
    ui_->legsWidget->setLegs(legs);
    hasChanges_ = saved;
    updateSaveButtonState();
}

void CompositeInstrumentDetailDialog::loadLegs() {
    if (!clientManager_ || instrument_.id.is_nil()) return;

    QPointer<CompositeInstrumentDetailDialog> self = this;
    const auto instrument_id = boost::uuids::to_string(instrument_.id);

    struct LegsResult {
        bool success;
        std::string message;
        std::vector<trading::domain::composite_leg> legs;
    };

    auto task = [self, instrument_id]() -> LegsResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed", {}};

        trading::messaging::get_composite_instrument_legs_request request;
        request.instrument_id = instrument_id;
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result)
            return {false, "Failed to communicate with server", {}};

        return {response_result->success, response_result->message,
                std::move(response_result->legs)};
    };

    auto* watcher = new QFutureWatcher<LegsResult>(self);
    connect(watcher, &QFutureWatcher<LegsResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            self->setLegs(result.legs);
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load composite legs: " << result.message;
        }
    });

    QFuture<LegsResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CompositeInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CompositeInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->legsWidget->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CompositeInstrumentDetailDialog::updateUiFromCompositeInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));

    populateProvenance(instrument_.version,
                       instrument_.modified_by,
                       instrument_.performed_by,
                       instrument_.recorded_at,
                       instrument_.change_reason_code,
                       instrument_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CompositeInstrumentDetailDialog::updateCompositeInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CompositeInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CompositeInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool CompositeInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty();
}

void CompositeInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save composite instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateCompositeInstrumentFromUi();
    const auto legs = ui_->legsWidget->legs();

    BOOST_LOG_SEV(lg(), info) << "Saving composite instrument: "
                              << boost::uuids::to_string(instrument_.id)
                              << " with " << legs.size() << " legs";

    QPointer<CompositeInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_,
                 legs = legs]() -> SaveResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};

        trading::messaging::save_composite_instrument_request request;
        request.data = instrument;
        request.legs = legs;
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result)
            return {false, "Failed to communicate with server"};

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            QString id = QString::fromStdString(
                boost::uuids::to_string(self->instrument_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->compositeInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Composite instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CompositeInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete composite instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Composite Instrument",
        QString("Are you sure you want to delete composite instrument '%1'?")
            .arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<CompositeInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 id_str = boost::uuids::to_string(instrument_.id)]()
                -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};

        trading::messaging::delete_composite_instrument_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result)
            return {false, "Failed to communicate with server"};

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, id, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            emit self->compositeInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Composite instrument '%1' deleted").arg(id));
            self->requestClose();
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
