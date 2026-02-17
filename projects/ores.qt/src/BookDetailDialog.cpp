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
#include "ores.qt/BookDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_BookDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BookDetailDialog::BookDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BookDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BookDetailDialog::~BookDetailDialog() {
    delete ui_;
}

void BookDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void BookDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BookDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BookDetailDialog::onDeleteClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onCodeChanged);
    connect(ui_->ledgerCcyEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->glAccountRefEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->costCenterEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->bookStatusEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
}

void BookDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BookDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    if (imageCache_) {
        connect(imageCache_, &ImageCache::allLoaded, this,
                &BookDetailDialog::updateFlagIcons);
        connect(ui_->ledgerCcyEdit, &QLineEdit::textChanged, this,
                &BookDetailDialog::updateFlagIcons);
        updateFlagIcons();
    }
}

void BookDetailDialog::updateFlagIcons() {
    if (!imageCache_) return;
    const auto ccy = ui_->ledgerCcyEdit->text().trimmed().toStdString();
    QIcon icon = imageCache_->getCurrencyFlagIcon(ccy);
    set_line_edit_flag_icon(ui_->ledgerCcyEdit, icon, ledgerCcyFlagAction_);
}

void BookDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BookDetailDialog::setBook(
    const refdata::domain::book& book) {
    book_ = book;
    updateUiFromBook();
}

void BookDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->ledgerCcyEdit->setReadOnly(readOnly);
    ui_->glAccountRefEdit->setReadOnly(readOnly);
    ui_->costCenterEdit->setReadOnly(readOnly);
    ui_->bookStatusEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BookDetailDialog::updateUiFromBook() {
    ui_->nameEdit->setText(QString::fromStdString(book_.name));
    ui_->ledgerCcyEdit->setText(QString::fromStdString(book_.ledger_ccy));
    ui_->glAccountRefEdit->setText(QString::fromStdString(book_.gl_account_ref));
    ui_->costCenterEdit->setText(QString::fromStdString(book_.cost_center));
    ui_->bookStatusEdit->setText(QString::fromStdString(book_.book_status));

    ui_->versionEdit->setText(QString::number(book_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(book_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(book_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(book_.change_commentary));
}

void BookDetailDialog::updateBookFromUi() {
    if (createMode_) {
        book_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    book_.ledger_ccy = ui_->ledgerCcyEdit->text().trimmed().toStdString();
    book_.gl_account_ref = ui_->glAccountRefEdit->text().trimmed().toStdString();
    book_.cost_center = ui_->costCenterEdit->text().trimmed().toStdString();
    book_.book_status = ui_->bookStatusEdit->text().trimmed().toStdString();
    book_.modified_by = username_;
    book_.performed_by = username_;
}

void BookDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BookDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !name_val.isEmpty();
}

void BookDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save book while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateBookFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving book: " << book_.name;

    QPointer<BookDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, book = book_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_book_request request;
        request.book = book;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_book_request,
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

        auto response = refdata::messaging::save_book_response::
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
            BOOST_LOG_SEV(lg(), info) << "Book saved successfully";
            QString code = QString::fromStdString(self->book_.name);
            emit self->bookSaved(code);
            self->notifySaveSuccess(tr("Book '%1' saved").arg(code));
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

void BookDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete book while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(book_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Book",
        QString("Are you sure you want to delete book '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting book: " << book_.name;

    QPointer<BookDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = book_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_book_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_book_request,
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

        auto response = refdata::messaging::delete_book_response::
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
            BOOST_LOG_SEV(lg(), info) << "Book deleted successfully";
            emit self->statusMessage(QString("Book '%1' deleted").arg(code));
            emit self->bookDeleted(code);
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
