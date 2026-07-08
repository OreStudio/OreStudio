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
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ui_BookDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

BookDetailDialog::BookDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::BookDetailDialog)
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

BookDetailDialog::~BookDetailDialog() {
    delete ui_;
}

QTabWidget* BookDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BookDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BookDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void BookDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BookDetailDialog::setupCombos() {
    ui_->bookStatusCombo->clear();
    ui_->bookStatusCombo->addItem(tr("Active"), QString("Active"));
    ui_->bookStatusCombo->addItem(tr("Inactive"), QString("Inactive"));
    ui_->bookStatusCombo->addItem(tr("Closed"), QString("Closed"));
    ui_->bookStatusCombo->addItem(tr("Frozen"), QString("Frozen"));
    ui_->bookStatusCombo->addItem(tr("Pending"), QString("Pending"));
}

void BookDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &BookDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &BookDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &BookDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->ledgerCcyEdit,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
    connect(
        ui_->glAccountRefEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->costCenterEdit, &QLineEdit::textChanged, this, &BookDetailDialog::onFieldChanged);
    connect(ui_->bookStatusCombo,
            &QComboBox::currentIndexChanged,
            this,
            &BookDetailDialog::onFieldChanged);
}

void BookDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    setup_badge_combo(this, ui_->bookStatusCombo, badgeCache(), "book_status");
    populateLedgerCcyCombo();
}

void BookDetailDialog::populateLedgerCcyCombo() {
    setup_currency_combo(ui_->ledgerCcyEdit, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(book_.ledger_ccy);
    });
}

void BookDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BookDetailDialog::setBook(const refdata::domain::book& book) {
    book_ = book;
    updateUiFromBook();
}

void BookDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        book_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BookDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->ledgerCcyEdit->setEnabled(!readOnly);
    ui_->glAccountRefEdit->setReadOnly(readOnly);
    ui_->costCenterEdit->setReadOnly(readOnly);
    ui_->bookStatusCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BookDetailDialog::updateUiFromBook() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(book_.id)));
    ui_->nameEdit->setText(QString::fromStdString(book_.name));
    {
        const auto val = QString::fromStdString(book_.ledger_ccy);
        const int idx = ui_->ledgerCcyEdit->findText(val);
        ui_->ledgerCcyEdit->setCurrentIndex(idx);
    }
    ui_->glAccountRefEdit->setText(QString::fromStdString(book_.gl_account_ref));
    ui_->costCenterEdit->setText(QString::fromStdString(book_.cost_center));
    {
        const int idx = ui_->bookStatusCombo->findData(QString::fromStdString(book_.book_status));
        if (idx >= 0)
            ui_->bookStatusCombo->setCurrentIndex(idx);
    }
    ui_->isTradingBookCheck->setChecked(book_.is_trading_book);

    populateProvenance(book_.version,
                       book_.modified_by,
                       book_.performed_by,
                       book_.recorded_at,
                       book_.change_reason_code,
                       book_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BookDetailDialog::updateBookFromUi() {
    book_.name = ui_->nameEdit->text().trimmed().toStdString();
    book_.ledger_ccy = ui_->ledgerCcyEdit->currentText().toStdString();
    book_.gl_account_ref = ui_->glAccountRefEdit->text().trimmed().toStdString();
    book_.cost_center = ui_->costCenterEdit->text().trimmed().toStdString();
    book_.book_status = ui_->bookStatusCombo->currentData().toString().toStdString();
    book_.is_trading_book = ui_->isTradingBookCheck->isChecked();
    book_.modified_by = username_;
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
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !id_val.isEmpty() && !name_val.isEmpty();
}

void BookDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save book while disconnected from server.");
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
    book_.change_reason_code = crSel->reason_code;
    book_.change_commentary = crSel->commentary;

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
        request.data = book;
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
            BOOST_LOG_SEV(lg(), info) << "Book saved successfully";
            QString code = QString::fromStdString(self->book_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
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
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete book while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(book_.name);
    auto reply =
        MessageBoxHelper::question(this,
                                   "Delete Book",
                                   QString("Are you sure you want to delete book '%1'?").arg(code),
                                   QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting book: " << book_.name;

    QPointer<BookDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(book_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_book_request request;
        request.ids = {id_str};
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
