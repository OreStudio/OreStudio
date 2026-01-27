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
#include "ores.qt/DatasetBundleDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_DatasetBundleDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/dataset_bundle_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

DatasetBundleDetailDialog::DatasetBundleDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::DatasetBundleDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

DatasetBundleDetailDialog::~DatasetBundleDetailDialog() {
    delete ui_;
}

void DatasetBundleDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void DatasetBundleDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &DatasetBundleDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &DatasetBundleDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &DatasetBundleDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &DatasetBundleDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
            &DatasetBundleDetailDialog::onFieldChanged);
}

void DatasetBundleDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void DatasetBundleDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void DatasetBundleDetailDialog::setBundle(
    const dq::domain::dataset_bundle& bundle) {
    bundle_ = bundle;
    updateUiFromBundle();
}

void DatasetBundleDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void DatasetBundleDetailDialog::updateUiFromBundle() {
    ui_->codeEdit->setText(QString::fromStdString(bundle_.code));
    ui_->nameEdit->setText(QString::fromStdString(bundle_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(bundle_.description));

    ui_->versionEdit->setText(QString::number(bundle_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(bundle_.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(bundle_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(bundle_.change_commentary));
}

void DatasetBundleDetailDialog::updateBundleFromUi() {
    if (createMode_) {
        bundle_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    bundle_.name = ui_->nameEdit->text().trimmed().toStdString();
    bundle_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    bundle_.recorded_by = username_;
}

void DatasetBundleDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool DatasetBundleDetailDialog::validateInput() {
    const QString code = ui_->codeEdit->text().trimmed();
    const QString name = ui_->nameEdit->text().trimmed();

    return !code.isEmpty() && !name.isEmpty();
}

void DatasetBundleDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save dataset bundle while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (Code and Name).");
        return;
    }

    updateBundleFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving dataset bundle: " << bundle_.code;

    QPointer<DatasetBundleDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, bundle = bundle_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_dataset_bundle_request request;
        request.bundle = bundle;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_dataset_bundle_request,
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

        auto response = dq::messaging::save_dataset_bundle_response::
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
            BOOST_LOG_SEV(lg(), info) << "Dataset Bundle saved successfully";
            QString code = QString::fromStdString(self->bundle_.code);
            emit self->bundleSaved(code);
            self->notifySaveSuccess(tr("Dataset Bundle '%1' saved").arg(code));
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

void DatasetBundleDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete dataset bundle while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(bundle_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Dataset Bundle",
        QString("Are you sure you want to delete dataset bundle '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting dataset bundle: " << bundle_.code;

    QPointer<DatasetBundleDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = bundle_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_dataset_bundle_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_dataset_bundle_request,
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

        auto response = dq::messaging::delete_dataset_bundle_response::
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
            BOOST_LOG_SEV(lg(), info) << "Dataset Bundle deleted successfully";
            emit self->statusMessage(QString("Dataset Bundle '%1' deleted").arg(code));
            emit self->bundleDeleted(code);
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
