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
#include "ores.qt/OriginDimensionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_OriginDimensionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

OriginDimensionDetailDialog::OriginDimensionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::OriginDimensionDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

OriginDimensionDetailDialog::~OriginDimensionDetailDialog() {
    delete ui_;
}

void OriginDimensionDetailDialog::setupUi() {
    const auto& iconColor = color_constants::icon_color;

    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_save_20_regular.svg", iconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor));
}

void OriginDimensionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &OriginDimensionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &OriginDimensionDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &OriginDimensionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &OriginDimensionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
            &OriginDimensionDetailDialog::onFieldChanged);
}

void OriginDimensionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void OriginDimensionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void OriginDimensionDetailDialog::setDimension(
    const dq::domain::origin_dimension& dimension) {
    dimension_ = dimension;
    updateUiFromDimension();
}

void OriginDimensionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void OriginDimensionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void OriginDimensionDetailDialog::updateUiFromDimension() {
    ui_->codeEdit->setText(QString::fromStdString(dimension_.code));
    ui_->nameEdit->setText(QString::fromStdString(dimension_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(dimension_.description));

    ui_->versionEdit->setText(QString::number(dimension_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(dimension_.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(dimension_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(dimension_.change_commentary));
}

void OriginDimensionDetailDialog::updateDimensionFromUi() {
    if (createMode_) {
        dimension_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    dimension_.name = ui_->nameEdit->text().trimmed().toStdString();
    dimension_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    dimension_.recorded_by = username_;
}

void OriginDimensionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OriginDimensionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void OriginDimensionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool OriginDimensionDetailDialog::validateInput() {
    const QString code = ui_->codeEdit->text().trimmed();
    const QString name = ui_->nameEdit->text().trimmed();

    return !code.isEmpty() && !name.isEmpty();
}

void OriginDimensionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save origin dimension while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (Code and Name).");
        return;
    }

    updateDimensionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving origin dimension: " << dimension_.code;

    QPointer<OriginDimensionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, dimension = dimension_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_origin_dimension_request request;
        request.dimension = dimension;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_origin_dimension_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = dq::messaging::save_origin_dimension_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Origin dimension saved successfully";
            QString code = QString::fromStdString(self->dimension_.code);
            emit self->dimensionSaved(code);
            self->notifySaveSuccess(tr("Origin dimension '%1' saved").arg(code));
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

void OriginDimensionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete origin dimension while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(dimension_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Origin Dimension",
        QString("Are you sure you want to delete origin dimension '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting origin dimension: " << dimension_.code;

    QPointer<OriginDimensionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = dimension_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_origin_dimension_request request;
        request.codes = {code};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_origin_dimension_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = dq::messaging::delete_origin_dimension_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Origin dimension deleted successfully";
            emit self->statusMessage(QString("Origin dimension '%1' deleted").arg(code));
            emit self->dimensionDeleted(code);
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
