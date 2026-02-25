/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/NatureDimensionDetailDialog.hpp"

#include <QPlainTextEdit>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_NatureDimensionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

NatureDimensionDetailDialog::NatureDimensionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::NatureDimensionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

NatureDimensionDetailDialog::~NatureDimensionDetailDialog() {
    delete ui_;
}

QTabWidget* NatureDimensionDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* NatureDimensionDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* NatureDimensionDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void NatureDimensionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void NatureDimensionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &NatureDimensionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &NatureDimensionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &NatureDimensionDetailDialog::onCloseClicked);
    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &NatureDimensionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &NatureDimensionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, &NatureDimensionDetailDialog::onFieldChanged);
}

void NatureDimensionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void NatureDimensionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void NatureDimensionDetailDialog::setDimension(const dq::domain::nature_dimension& dimension) {
    dimension_ = dimension;
    updateUiFromDimension();
}

void NatureDimensionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void NatureDimensionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void NatureDimensionDetailDialog::updateUiFromDimension() {
    ui_->codeEdit->setText(QString::fromStdString(dimension_.code));
    ui_->nameEdit->setText(QString::fromStdString(dimension_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(dimension_.description));
    populateProvenance(dimension_.version, dimension_.modified_by, dimension_.performed_by,
                       dimension_.recorded_at, "", dimension_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void NatureDimensionDetailDialog::updateDimensionFromUi() {
    if (createMode_) dimension_.code = ui_->codeEdit->text().trimmed().toStdString();
    dimension_.name = ui_->nameEdit->text().trimmed().toStdString();
    dimension_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    dimension_.modified_by = username_;
}

void NatureDimensionDetailDialog::onCodeChanged(const QString&) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void NatureDimensionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void NatureDimensionDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool NatureDimensionDetailDialog::validateInput() {
    return !ui_->codeEdit->text().trimmed().isEmpty() && !ui_->nameEdit->text().trimmed().isEmpty();
}

void NatureDimensionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected", "Cannot save while disconnected.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in Code and Name.");
        return;
    }

    updateDimensionFromUi();
    BOOST_LOG_SEV(lg(), info) << "Saving nature dimension: " << dimension_.code;

    QPointer<NatureDimensionDetailDialog> self = this;
    struct SaveResult { bool success; std::string message; };

    auto task = [self, dimension = dimension_]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_nature_dimension_request request;
        request.dimension = dimension;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_nature_dimension_request, 0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_nature_dimension_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            QString code = QString::fromStdString(self->dimension_.code);
            emit self->dimensionSaved(code);
            self->notifySaveSuccess(tr("Nature dimension '%1' saved").arg(code));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void NatureDimensionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected", "Cannot delete while disconnected.");
        return;
    }

    QString code = QString::fromStdString(dimension_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Nature Dimension",
        QString("Are you sure you want to delete '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting nature dimension: " << dimension_.code;

    QPointer<NatureDimensionDetailDialog> self = this;
    struct DeleteResult { bool success; std::string message; };

    auto task = [self, code = dimension_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::delete_nature_dimension_request request;
        request.codes = {code};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_nature_dimension_request, 0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::delete_nature_dimension_response::deserialize(*payload_result);
        if (!response || response->results.empty()) return {false, "Invalid server response"};

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            emit self->statusMessage(QString("Nature dimension '%1' deleted").arg(code));
            emit self->dimensionDeleted(code);
            self->requestClose();
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

}
