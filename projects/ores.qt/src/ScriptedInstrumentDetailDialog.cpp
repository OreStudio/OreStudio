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
#include "ores.qt/ScriptedInstrumentDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_ScriptedInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ScriptedInstrumentDetailDialog::ScriptedInstrumentDetailDialog(
    QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::ScriptedInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

ScriptedInstrumentDetailDialog::~ScriptedInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* ScriptedInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* ScriptedInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* ScriptedInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void ScriptedInstrumentDetailDialog::setupUi() {
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

void ScriptedInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &ScriptedInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &ScriptedInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &ScriptedInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->scriptNameEdit, &QLineEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->scriptBodyEdit, &QPlainTextEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->eventsJsonEdit, &QPlainTextEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->underlyingsJsonEdit, &QPlainTextEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
    connect(ui_->parametersJsonEdit, &QPlainTextEdit::textChanged, this,
            &ScriptedInstrumentDetailDialog::onFieldChanged);
}

void ScriptedInstrumentDetailDialog::setClientManager(
    ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ScriptedInstrumentDetailDialog::setUsername(
    const std::string& username) {
    username_ = username;
}

void ScriptedInstrumentDetailDialog::setScriptedInstrument(
    const trading::domain::scripted_instrument& v) {
    instrument_ = v;
    updateUiFromScriptedInstrument();
}

void ScriptedInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void ScriptedInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->scriptNameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->scriptBodyEdit->setReadOnly(readOnly);
    ui_->eventsJsonEdit->setReadOnly(readOnly);
    ui_->underlyingsJsonEdit->setReadOnly(readOnly);
    ui_->parametersJsonEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void ScriptedInstrumentDetailDialog::updateUiFromScriptedInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->scriptNameEdit->setText(
        QString::fromStdString(instrument_.script_name));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->scriptBodyEdit->setPlainText(
        QString::fromStdString(instrument_.script_body));
    ui_->eventsJsonEdit->setPlainText(
        QString::fromStdString(instrument_.events_json));
    ui_->underlyingsJsonEdit->setPlainText(
        QString::fromStdString(instrument_.underlyings_json));
    ui_->parametersJsonEdit->setPlainText(
        QString::fromStdString(instrument_.parameters_json));

    populateProvenance(instrument_.version,
                       instrument_.modified_by,
                       instrument_.performed_by,
                       instrument_.recorded_at,
                       instrument_.change_reason_code,
                       instrument_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void ScriptedInstrumentDetailDialog::updateScriptedInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.script_name =
        ui_->scriptNameEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.script_body =
        ui_->scriptBodyEdit->toPlainText().trimmed().toStdString();
    instrument_.events_json =
        ui_->eventsJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.underlyings_json =
        ui_->underlyingsJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.parameters_json =
        ui_->parametersJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void ScriptedInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void ScriptedInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool ScriptedInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->scriptNameEdit->text().trimmed().isEmpty();
}

void ScriptedInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save scripted instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateScriptedInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving scripted instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<ScriptedInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_scripted_instrument_request request;
        request.data = instrument;
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

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
            QString id = QString::fromStdString(
                boost::uuids::to_string(self->instrument_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->scriptedInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Scripted instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void ScriptedInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete scripted instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Scripted Instrument",
        QString("Are you sure you want to delete scripted instrument '%1'?")
            .arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<ScriptedInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 id_str = boost::uuids::to_string(instrument_.id)]()
                -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_scripted_instrument_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, id, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            emit self->scriptedInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Scripted instrument '%1' deleted").arg(id));
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
