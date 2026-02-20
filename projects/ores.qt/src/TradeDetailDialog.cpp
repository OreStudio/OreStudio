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
#include "ores.qt/TradeDetailDialog.hpp"

#include <QComboBox>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_TradeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.trade/messaging/trade_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TradeDetailDialog::TradeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::TradeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

TradeDetailDialog::~TradeDetailDialog() {
    delete ui_;
}

void TradeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void TradeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onDeleteClicked);

    connect(ui_->bookCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TradeDetailDialog::onFieldChanged);
    connect(ui_->externalIdEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCodeChanged);
    connect(ui_->tradeTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->lifecycleEventEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->nettingSetIdEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->tradeDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->effectiveDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->terminationDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->executionTimestampEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
}

void TradeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadBooks();
}

void TradeDetailDialog::loadBooks() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    BOOST_LOG_SEV(lg(), debug) << "Loading books for trade dialog";

    struct BooksResult {
        bool success;
        std::vector<refdata::domain::book> books;
    };

    QPointer<TradeDetailDialog> self = this;
    QFuture<BooksResult> future = QtConcurrent::run([self]() -> BooksResult {
        if (!self || !self->clientManager_)
            return {false, {}};

        refdata::messaging::get_books_request request;
        request.offset = 0;
        request.limit = 1000;
        auto payload = request.serialize();

        comms::messaging::frame req_frame(
            comms::messaging::message_type::get_books_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(req_frame));
        if (!response_result) return {false, {}};

        auto decompressed = response_result->decompressed_payload();
        if (!decompressed) return {false, {}};

        auto response = refdata::messaging::get_books_response::deserialize(*decompressed);
        if (!response) return {false, {}};

        return {true, std::move(response->books)};
    });

    auto* watcher = new QFutureWatcher<BooksResult>(self);
    connect(watcher, &QFutureWatcher<BooksResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->books_ = std::move(result.books);

        self->ui_->bookCombo->blockSignals(true);
        self->ui_->bookCombo->clear();
        for (const auto& book : self->books_) {
            QString name = QString::fromStdString(book.name);
            QString id = QString::fromStdString(boost::uuids::to_string(book.id));
            self->ui_->bookCombo->addItem(name, id);
        }
        self->ui_->bookCombo->blockSignals(false);

        BOOST_LOG_SEV(lg(), debug) << "Loaded " << self->books_.size() << " books";
        self->selectCurrentBook();
    });

    watcher->setFuture(future);
}

void TradeDetailDialog::selectCurrentBook() {
    if (trade_.book_id.is_nil()) return;

    const QString target = QString::fromStdString(boost::uuids::to_string(trade_.book_id));
    for (int i = 0; i < ui_->bookCombo->count(); ++i) {
        if (ui_->bookCombo->itemData(i).toString() == target) {
            ui_->bookCombo->blockSignals(true);
            ui_->bookCombo->setCurrentIndex(i);
            ui_->bookCombo->blockSignals(false);
            return;
        }
    }
}

void TradeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TradeDetailDialog::setTrade(
    const trade::domain::trade& trade) {
    trade_ = trade;
    updateUiFromTrade();
    // Select book if already loaded; otherwise selectCurrentBook() runs
    // again after loadBooks() completes via the watcher callback.
    selectCurrentBook();
}

void TradeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->bookCombo->setEnabled(createMode);
    ui_->externalIdEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        trade_.id = boost::uuids::random_generator()();
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void TradeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->bookCombo->setEnabled(!readOnly);
    ui_->externalIdEdit->setReadOnly(true);
    ui_->tradeTypeEdit->setReadOnly(readOnly);
    ui_->lifecycleEventEdit->setReadOnly(readOnly);
    ui_->nettingSetIdEdit->setReadOnly(readOnly);
    ui_->tradeDateEdit->setReadOnly(readOnly);
    ui_->effectiveDateEdit->setReadOnly(readOnly);
    ui_->terminationDateEdit->setReadOnly(readOnly);
    ui_->executionTimestampEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TradeDetailDialog::updateUiFromTrade() {
    ui_->externalIdEdit->setText(QString::fromStdString(trade_.external_id));
    ui_->tradeTypeEdit->setText(QString::fromStdString(trade_.trade_type));
    ui_->lifecycleEventEdit->setText(QString::fromStdString(trade_.lifecycle_event));
    ui_->nettingSetIdEdit->setText(QString::fromStdString(trade_.netting_set_id));
    ui_->tradeDateEdit->setText(QString::fromStdString(trade_.trade_date));
    ui_->effectiveDateEdit->setText(QString::fromStdString(trade_.effective_date));
    ui_->terminationDateEdit->setText(QString::fromStdString(trade_.termination_date));
    ui_->executionTimestampEdit->setText(QString::fromStdString(trade_.execution_timestamp));

    ui_->versionEdit->setText(QString::number(trade_.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(trade_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(trade_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(trade_.change_commentary));
}

void TradeDetailDialog::updateTradeFromUi() {
    // Read book selection and derive portfolio_id from it
    const int bookIdx = ui_->bookCombo->currentIndex();
    if (bookIdx >= 0 && bookIdx < static_cast<int>(books_.size())) {
        const auto& book = books_[static_cast<std::size_t>(bookIdx)];
        trade_.book_id = book.id;
        trade_.portfolio_id = book.parent_portfolio_id;
    }

    if (createMode_) {
        trade_.external_id = ui_->externalIdEdit->text().trimmed().toStdString();
    }
    trade_.trade_type = ui_->tradeTypeEdit->text().trimmed().toStdString();
    trade_.lifecycle_event = ui_->lifecycleEventEdit->text().trimmed().toStdString();
    trade_.netting_set_id = ui_->nettingSetIdEdit->text().trimmed().toStdString();
    trade_.trade_date = ui_->tradeDateEdit->text().trimmed().toStdString();
    trade_.effective_date = ui_->effectiveDateEdit->text().trimmed().toStdString();
    trade_.termination_date = ui_->terminationDateEdit->text().trimmed().toStdString();
    trade_.execution_timestamp = ui_->executionTimestampEdit->text().trimmed().toStdString();
    trade_.modified_by = username_;
    trade_.performed_by = username_;
}

void TradeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TradeDetailDialog::validateInput() {
    if (ui_->bookCombo->currentIndex() < 0) return false;

    const QString trade_type_val = ui_->tradeTypeEdit->text().trimmed();
    const QString lifecycle_event_val = ui_->lifecycleEventEdit->text().trimmed();
    const QString trade_date_val = ui_->tradeDateEdit->text().trimmed();
    const QString effective_date_val = ui_->effectiveDateEdit->text().trimmed();
    const QString termination_date_val = ui_->terminationDateEdit->text().trimmed();

    return !trade_type_val.isEmpty() && !lifecycle_event_val.isEmpty() && !trade_date_val.isEmpty() && !effective_date_val.isEmpty() && !termination_date_val.isEmpty();
}

void TradeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save trade while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTradeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving trade: " << trade_.external_id;

    QPointer<TradeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, trade = trade_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trade::messaging::save_trade_request request;
        request.trade = trade;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_trade_request,
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

        auto response = trade::messaging::save_trade_response::
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
            BOOST_LOG_SEV(lg(), info) << "Trade saved successfully";
            QString code = QString::fromStdString(self->trade_.external_id);
            emit self->tradeSaved(code);
            self->notifySaveSuccess(tr("Trade '%1' saved").arg(code));
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

void TradeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete trade while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(trade_.external_id);
    auto reply = MessageBoxHelper::question(this, "Delete Trade",
        QString("Are you sure you want to delete trade '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting trade: " << trade_.external_id;

    QPointer<TradeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = trade_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trade::messaging::delete_trade_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_trade_request,
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

        auto response = trade::messaging::delete_trade_response::
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
            BOOST_LOG_SEV(lg(), info) << "Trade deleted successfully";
            emit self->statusMessage(QString("Trade '%1' deleted").arg(code));
            emit self->tradeDeleted(code);
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
