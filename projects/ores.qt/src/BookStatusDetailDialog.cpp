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
#include "ores.qt/BookStatusDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_BookStatusDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata/messaging/book_status_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BookStatusDetailDialog::BookStatusDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BookStatusDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BookStatusDetailDialog::~BookStatusDetailDialog() {
    delete ui_;
}

QTabWidget* BookStatusDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* BookStatusDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* BookStatusDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void BookStatusDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void BookStatusDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BookStatusDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BookStatusDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BookStatusDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BookStatusDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
            &BookStatusDetailDialog::onFieldChanged);
}

void BookStatusDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BookStatusDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BookStatusDetailDialog::setStatus(
    const refdata::domain::book_status& status) {
    status_ = status;
    updateUiFromStatus();
}

void BookStatusDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BookStatusDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BookStatusDetailDialog::updateUiFromStatus() {
    ui_->codeEdit->setText(QString::fromStdString(status_.code));
    ui_->nameEdit->setText(QString::fromStdString(status_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(status_.description));

    populateProvenance(status_.version, status_.modified_by, status_.performed_by,
                       status_.recorded_at, status_.change_reason_code,
                       status_.change_commentary);
}

void BookStatusDetailDialog::updateStatusFromUi() {
    if (createMode_) {
        status_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    status_.name = ui_->nameEdit->text().trimmed().toStdString();
    status_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    status_.modified_by = username_;
    status_.performed_by = username_;
}

void BookStatusDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookStatusDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookStatusDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BookStatusDetailDialog::validateInput() {
    const QString code = ui_->codeEdit->text().trimmed();
    const QString name = ui_->nameEdit->text().trimmed();

    return !code.isEmpty() && !name.isEmpty();
}

void BookStatusDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save book status while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (Code and Name).");
        return;
    }

    updateStatusFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving book status: " << status_.code;

    QPointer<BookStatusDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, status = status_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_book_status_request request;
        request.status = status;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_book_status_request,
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

        auto response = refdata::messaging::save_book_status_response::
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
            BOOST_LOG_SEV(lg(), info) << "Book Status saved successfully";
            QString code = QString::fromStdString(self->status_.code);
            emit self->statusSaved(code);
            self->notifySaveSuccess(tr("Book Status '%1' saved").arg(code));
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

void BookStatusDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete book status while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(status_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Book Status",
        QString("Are you sure you want to delete book status '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting book status: " << status_.code;

    QPointer<BookStatusDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = status_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_book_status_request request;
        request.codes = {code};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_book_status_request,
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

        auto response = refdata::messaging::delete_book_status_response::
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
            BOOST_LOG_SEV(lg(), info) << "Book Status deleted successfully";
            emit self->statusMessage(QString("Book Status '%1' deleted").arg(code));
            emit self->statusDeleted(code);
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
