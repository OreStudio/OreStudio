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
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_BookDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BookDetailDialog::BookDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BookDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

BookDetailDialog::~BookDetailDialog() {
    delete ui_;
}

QTabWidget* BookDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* BookDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* BookDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

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
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->ledgerCcyCombo, &QComboBox::currentTextChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->glAccountRefEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->costCenterEdit, &QLineEdit::textChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->bookStatusCombo, &QComboBox::currentTextChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->bookTypeCombo, &QComboBox::currentTextChanged, this,
            &BookDetailDialog::onFieldChanged);
    connect(ui_->parentPortfolioCombo, &QComboBox::currentTextChanged, this,
            &BookDetailDialog::onFieldChanged);
}

void BookDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCurrencyCombo();
    populateBookStatusCombo();
    populateParentPortfolioCombo();
}

void BookDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    if (imageCache_) {
        connect(imageCache_, &ImageCache::allLoaded, this, [this]() {
            set_combo_flag_icons(ui_->ledgerCcyCombo,
                [this](const std::string& code) {
                    return imageCache_->getCurrencyFlagIcon(code);
                });
        });
        set_combo_flag_icons(ui_->ledgerCcyCombo,
            [this](const std::string& code) {
                return imageCache_->getCurrencyFlagIcon(code);
            });
    }
}

void BookDetailDialog::populateCurrencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<BookDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        return fetch_currency_codes(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished,
            self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->ui_->ledgerCcyCombo->clear();
        for (const auto& code : codes) {
            self->ui_->ledgerCcyCombo->addItem(
                QString::fromStdString(code));
        }

        if (self->imageCache_) {
            set_combo_flag_icons(self->ui_->ledgerCcyCombo,
                [&self](const std::string& code) {
                    return self->imageCache_->getCurrencyFlagIcon(code);
                });
        }

        self->updateUiFromBook();
    });

    QFuture<std::vector<std::string>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BookDetailDialog::populateBookStatusCombo() {
    ui_->bookStatusCombo->clear();
    // Lifecycle states for books: Active, Closed, Frozen.
    // TODO: populate asynchronously from the book_statuses lookup service
    // once the protocol is available.
    for (const auto* status : {"Active", "Closed", "Frozen"}) {
        ui_->bookStatusCombo->addItem(QString::fromUtf8(status));
    }
}

void BookDetailDialog::populateParentPortfolioCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<BookDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<portfolio_entry> {
        return fetch_portfolio_entries(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<portfolio_entry>>(self);
    connect(watcher, &QFutureWatcher<std::vector<portfolio_entry>>::finished,
            self, [self, watcher]() {
        auto entries = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->portfolioEntries_ = entries;
        self->ui_->parentPortfolioCombo->clear();
        for (const auto& e : entries) {
            self->ui_->parentPortfolioCombo->addItem(
                QString::fromStdString(e.name));
        }
        self->updateUiFromBook();
    });

    QFuture<std::vector<portfolio_entry>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
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
    if (createMode) {
        boost::uuids::random_generator gen;
        book_.id = gen();
    }
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->ledgerCcyCombo->setEnabled(!readOnly);
    ui_->glAccountRefEdit->setReadOnly(readOnly);
    ui_->costCenterEdit->setReadOnly(readOnly);
    ui_->bookStatusCombo->setEnabled(!readOnly);
    ui_->bookTypeCombo->setEnabled(!readOnly);
    ui_->parentPortfolioCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BookDetailDialog::updateUiFromBook() {
    ui_->nameEdit->setText(QString::fromStdString(book_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(book_.description));
    ui_->ledgerCcyCombo->setCurrentText(QString::fromStdString(book_.ledger_ccy));
    ui_->glAccountRefEdit->setText(QString::fromStdString(book_.gl_account_ref));
    ui_->costCenterEdit->setText(QString::fromStdString(book_.cost_center));
    ui_->bookStatusCombo->setCurrentText(QString::fromStdString(book_.book_status));
    ui_->bookTypeCombo->setCurrentIndex(book_.is_trading_book != 0 ? 1 : 0);

    // Select the parent portfolio by matching the stored UUID to a loaded entry
    const auto parent_id_str =
        boost::uuids::to_string(book_.parent_portfolio_id);
    ui_->parentPortfolioCombo->setCurrentIndex(0);
    for (const auto& e : portfolioEntries_) {
        if (e.id == parent_id_str) {
            ui_->parentPortfolioCombo->setCurrentText(
                QString::fromStdString(e.name));
            break;
        }
    }

    populateProvenance(book_.version, book_.modified_by, book_.performed_by,
                       book_.recorded_at, book_.change_reason_code,
                       book_.change_commentary);
}

void BookDetailDialog::updateBookFromUi() {
    if (createMode_) {
        book_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    book_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    book_.ledger_ccy = ui_->ledgerCcyCombo->currentText().trimmed().toStdString();
    book_.gl_account_ref = ui_->glAccountRefEdit->text().trimmed().toStdString();
    book_.cost_center = ui_->costCenterEdit->text().trimmed().toStdString();
    book_.book_status = ui_->bookStatusCombo->currentText().trimmed().toStdString();
    book_.is_trading_book = (ui_->bookTypeCombo->currentIndex() == 1) ? 1 : 0;
    book_.modified_by = username_;
    book_.performed_by = username_;

    // Resolve parent portfolio name back to UUID
    const auto parent_name =
        ui_->parentPortfolioCombo->currentText().trimmed().toStdString();
    for (const auto& e : portfolioEntries_) {
        if (e.name == parent_name) {
            book_.parent_portfolio_id =
                boost::lexical_cast<boost::uuids::uuid>(e.id);
            break;
        }
    }
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
