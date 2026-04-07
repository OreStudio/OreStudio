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
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_TradeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/trade_type_protocol.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// Local alias to keep dispatch sites concise without conflicting with the
// trade.product_type member name.
using PT = ores::trading::domain::product_type;

// ---------------------------------------------------------------------------
// Trade type detection helpers
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

TradeDetailDialog::TradeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::TradeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();

    // Zero-interval timer coalesces rapid tradeTypeEdit keystrokes so the
    // form selection logic runs at most once per event-loop cycle.
    createTypeTimer_ = new QTimer(this);
    createTypeTimer_->setSingleShot(true);
    createTypeTimer_->setInterval(0);
    connect(createTypeTimer_, &QTimer::timeout, this, [this]() {
        applyCreateTradeType();
    });

    // Build a stack page per registered IInstrumentForm. formMap_ is
    // populated here so setTrade() can reach any form in O(1).
    register_default_forms(instrumentFormRegistry_);
    for (const auto pt : instrumentFormRegistry_.registeredTypes()) {
        IInstrumentForm* form =
            instrumentFormRegistry_.createForm(pt, ui_->instrumentStack);
        ui_->instrumentStack->addWidget(form);
        formMap_[pt] = form;

        connect(form, &IInstrumentForm::changed, this,
                &TradeDetailDialog::onInstrumentFieldChanged);
        connect(form, &IInstrumentForm::instrumentLoaded, this, [this]() {
            instrumentLoaded_ = true;
            ui_->instrumentProvenanceGroup->setVisible(true);
            updateSaveButtonState();
        });
        connect(form, &IInstrumentForm::loadFailed, this,
                [](const QString& err) {
            BOOST_LOG_SEV(lg(), warn)
                << "Instrument load failed: " << err.toStdString();
        });
        connect(form, &IInstrumentForm::provenanceChanged, this,
                [this](const InstrumentProvenance& p) {
            ui_->instrumentProvenanceWidget->populate(
                p.version, p.modified_by, p.performed_by,
                p.recorded_at, p.change_reason_code, p.change_commentary);
            ui_->instrumentProvenanceGroup->setVisible(true);
        });
    }
}

TradeDetailDialog::~TradeDetailDialog() {
    delete ui_;
}

QTabWidget*       TradeDetailDialog::tabWidget()        const { return ui_->tabWidget; }
QWidget*          TradeDetailDialog::provenanceTab()    const { return ui_->provenanceTab; }
ProvenanceWidget* TradeDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

void TradeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);
    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // Hide all instrument tabs and instrument provenance section until an
    // instrument loads.
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->instrumentTab), false);
    ui_->instrumentProvenanceGroup->setVisible(false);
}

void TradeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onCloseClicked);

    // Trade fields
    connect(ui_->bookCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TradeDetailDialog::onFieldChanged);
    connect(ui_->counterpartyCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TradeDetailDialog::onFieldChanged);
    connect(ui_->externalIdEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCodeChanged);
    connect(ui_->tradeTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->tradeTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCreateTradeTypeChanged);
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


    // FX instrument fields are owned by FxInstrumentForm; signal wiring
    // happens in the dialog constructor when the form is added to the stack.
}

// ---------------------------------------------------------------------------
// Client manager and reference data loading
// ---------------------------------------------------------------------------

void TradeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadBooks();
    loadCounterparties();
    loadTradeTypes();
}

void TradeDetailDialog::loadBooks() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    struct BooksResult {
        bool success;
        std::vector<refdata::domain::book> books;
    };

    QPointer<TradeDetailDialog> self = this;
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
        self->selectCurrentBook();
    });

    watcher->setFuture(QtConcurrent::run([self]() -> BooksResult {
        if (!self || !self->clientManager_) return {false, {}};
        refdata::messaging::get_books_request req;
        req.offset = 0;
        req.limit = 1000;
        auto r = self->clientManager_->process_authenticated_request(std::move(req));
        if (!r) return {false, {}};
        return {true, std::move(r->books)};
    }));
}

void TradeDetailDialog::selectCurrentBook() {
    if (trade_.book_id.is_nil()) return;
    const QString target =
        QString::fromStdString(boost::uuids::to_string(trade_.book_id));
    for (int i = 0; i < ui_->bookCombo->count(); ++i) {
        if (ui_->bookCombo->itemData(i).toString() == target) {
            ui_->bookCombo->blockSignals(true);
            ui_->bookCombo->setCurrentIndex(i);
            ui_->bookCombo->blockSignals(false);
            return;
        }
    }
}

void TradeDetailDialog::loadCounterparties() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    struct CpResult {
        bool success;
        std::vector<refdata::domain::counterparty> counterparties;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CpResult>(self);
    connect(watcher, &QFutureWatcher<CpResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result.success) return;

        self->counterparties_ = std::move(result.counterparties);
        self->ui_->counterpartyCombo->blockSignals(true);
        self->ui_->counterpartyCombo->clear();
        self->ui_->counterpartyCombo->addItem(tr("-- None --"), QString());
        for (const auto& cp : self->counterparties_) {
            QString name = QString::fromStdString(cp.full_name);
            QString id = QString::fromStdString(boost::uuids::to_string(cp.id));
            self->ui_->counterpartyCombo->addItem(name, id);
        }
        self->ui_->counterpartyCombo->blockSignals(false);
        self->selectCurrentCounterparty();
    });

    watcher->setFuture(QtConcurrent::run([self]() -> CpResult {
        if (!self || !self->clientManager_) return {false, {}};
        refdata::messaging::get_counterparties_request req;
        req.offset = 0;
        req.limit = 1000;
        auto r = self->clientManager_->process_authenticated_request(std::move(req));
        if (!r) return {false, {}};
        return {true, std::move(r->counterparties)};
    }));
}

void TradeDetailDialog::selectCurrentCounterparty() {
    if (!trade_.counterparty_id.has_value()) {
        ui_->counterpartyCombo->blockSignals(true);
        ui_->counterpartyCombo->setCurrentIndex(0);
        ui_->counterpartyCombo->blockSignals(false);
        return;
    }
    const QString target =
        QString::fromStdString(boost::uuids::to_string(*trade_.counterparty_id));
    for (int i = 0; i < ui_->counterpartyCombo->count(); ++i) {
        if (ui_->counterpartyCombo->itemData(i).toString() == target) {
            ui_->counterpartyCombo->blockSignals(true);
            ui_->counterpartyCombo->setCurrentIndex(i);
            ui_->counterpartyCombo->blockSignals(false);
            return;
        }
    }
}

void TradeDetailDialog::loadTradeTypes() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    struct Result {
        bool success;
        std::vector<trading::domain::trade_type> types;
    };

    static constexpr int kTradeTypeFetchLimit = 1000;

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<Result>(self);
    connect(watcher, &QFutureWatcher<Result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to load trade types";
            return;
        }

        self->tradeTypeCache_.clear();
        for (auto& tt : result.types)
            self->tradeTypeCache_.emplace(tt.code, std::move(tt));
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() -> Result {
        if (!cm) return {false, {}};
        trading::messaging::get_trade_types_request req;
        req.limit = kTradeTypeFetchLimit;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, {}};
        return {true, std::move(r->types)};
    }));
}

// ---------------------------------------------------------------------------
// Public setters
// ---------------------------------------------------------------------------

void TradeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

// Selects @p form on the stack, sets client manager, username, and trade
// type flags from the cache.  Does not trigger load or clear — caller does
// that immediately after.
void TradeDetailDialog::activateForm(IInstrumentForm* form,
                                     const std::string& tradeTypeCode) {
    activeForm_ = form;
    ui_->instrumentStack->setCurrentWidget(form);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->instrumentTab), true);

    form->setClientManager(clientManager_);
    form->setUsername(username_);

    bool has_options = false, has_extension = false;
    auto it = tradeTypeCache_.find(tradeTypeCode);
    if (it != tradeTypeCache_.end()) {
        has_options   = it->second.has_options;
        has_extension = it->second.has_extension;
    }
    form->setTradeType(QString::fromStdString(tradeTypeCode),
                       has_options, has_extension);
}

void TradeDetailDialog::setTrade(const trading::domain::trade& trade) {
    trade_ = trade;
    updateUiFromTrade();
    selectCurrentBook();
    selectCurrentCounterparty();

    auto it = formMap_.find(trade_.product_type);
    if (it != formMap_.end()) {
        activateForm(it->second, trade_.trade_type);
        if (trade_.instrument_id.has_value()) {
            activeForm_->loadInstrument(
                boost::uuids::to_string(*trade_.instrument_id));
        } else {
            activeForm_->clear();
        }
    }
}

void TradeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->bookCombo->setEnabled(createMode);
    ui_->counterpartyCombo->setEnabled(createMode);
    ui_->externalIdEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode)
        trade_.id = boost::uuids::random_generator()();

    // Instrument tab starts hidden; onCreateTradeTypeChanged reveals it
    // once the user enters a known trade type code.
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->instrumentTab), false);
    ui_->instrumentProvenanceGroup->setVisible(false);
    activeForm_ = nullptr;
    instrumentLoaded_ = false;

    if (createMode)
        applyCreateTradeType();

    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    instrumentHasChanges_ = false;
    updateSaveButtonState();
}

void TradeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->bookCombo->setEnabled(!readOnly);
    ui_->counterpartyCombo->setEnabled(!readOnly);
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
    if (activeForm_) activeForm_->setReadOnly(readOnly);
}

// ---------------------------------------------------------------------------
// Trade UI helpers
// ---------------------------------------------------------------------------

void TradeDetailDialog::updateUiFromTrade() {
    ui_->externalIdEdit->setText(QString::fromStdString(trade_.external_id));
    ui_->tradeTypeEdit->setText(QString::fromStdString(trade_.trade_type));
    ui_->lifecycleEventEdit->setText(
        QString::fromStdString(trade_.activity_type_code));
    ui_->nettingSetIdEdit->setText(
        QString::fromStdString(trade_.netting_set_id));
    ui_->tradeDateEdit->setText(QString::fromStdString(trade_.trade_date));
    ui_->effectiveDateEdit->setText(
        QString::fromStdString(trade_.effective_date));
    ui_->terminationDateEdit->setText(
        QString::fromStdString(trade_.termination_date));
    ui_->executionTimestampEdit->setText(
        QString::fromStdString(trade_.execution_timestamp));

    populateProvenance(trade_.version, trade_.modified_by, trade_.performed_by,
                       trade_.recorded_at, trade_.change_reason_code,
                       trade_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TradeDetailDialog::updateTradeFromUi() {
    const int bookIdx = ui_->bookCombo->currentIndex();
    if (bookIdx >= 0 && bookIdx < static_cast<int>(books_.size())) {
        const auto& book = books_[static_cast<std::size_t>(bookIdx)];
        trade_.book_id = book.id;
        trade_.portfolio_id = book.parent_portfolio_id;
    }
    const int cpIdx = ui_->counterpartyCombo->currentIndex();
    if (cpIdx > 0) {
        const auto& cp = counterparties_[static_cast<std::size_t>(cpIdx - 1)];
        trade_.counterparty_id = cp.id;
    } else {
        trade_.counterparty_id = std::nullopt;
    }
    if (createMode_)
        trade_.external_id = ui_->externalIdEdit->text().trimmed().toStdString();
    trade_.trade_type =
        ui_->tradeTypeEdit->text().trimmed().toStdString();
    if (createMode_) {
        auto it = tradeTypeCache_.find(trade_.trade_type);
        if (it != tradeTypeCache_.end())
            trade_.product_type = it->second.product_type;
    }
    trade_.activity_type_code =
        ui_->lifecycleEventEdit->text().trimmed().toStdString();
    trade_.netting_set_id =
        ui_->nettingSetIdEdit->text().trimmed().toStdString();
    trade_.trade_date =
        ui_->tradeDateEdit->text().trimmed().toStdString();
    trade_.effective_date =
        ui_->effectiveDateEdit->text().trimmed().toStdString();
    trade_.termination_date =
        ui_->terminationDateEdit->text().trimmed().toStdString();
    trade_.execution_timestamp =
        ui_->executionTimestampEdit->text().trimmed().toStdString();
    trade_.modified_by = username_;
    trade_.performed_by = username_;
}

// ---------------------------------------------------------------------------
// Change tracking
// ---------------------------------------------------------------------------

void TradeDetailDialog::onCodeChanged(const QString&) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onInstrumentFieldChanged() {
    if (!instrumentLoaded_) return;
    instrumentHasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onCreateTradeTypeChanged(const QString&) {
    if (!createMode_) return;
    // Coalesce rapid keystrokes; applyCreateTradeType() fires once per cycle.
    createTypeTimer_->start();
}

void TradeDetailDialog::applyCreateTradeType() {
    const auto code = ui_->tradeTypeEdit->text().trimmed().toStdString();
    auto ttIt = tradeTypeCache_.find(code);
    if (ttIt == tradeTypeCache_.end()) {
        ui_->tabWidget->setTabVisible(
            ui_->tabWidget->indexOf(ui_->instrumentTab), false);
        activeForm_ = nullptr;
        return;
    }

    auto formIt = formMap_.find(ttIt->second.product_type);
    if (formIt == formMap_.end()) {
        ui_->tabWidget->setTabVisible(
            ui_->tabWidget->indexOf(ui_->instrumentTab), false);
        activeForm_ = nullptr;
        return;
    }

    // Only reset the form when the product family changes.
    if (activeForm_ != formIt->second) {
        activateForm(formIt->second, code);
        activeForm_->clear();
    }
}

void TradeDetailDialog::updateSaveButtonState() {
    const bool canSave =
        (hasChanges_ || instrumentHasChanges_) && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TradeDetailDialog::validateInput() {
    if (ui_->bookCombo->currentIndex() < 0) return false;
    return !ui_->tradeTypeEdit->text().trimmed().isEmpty() &&
           !ui_->lifecycleEventEdit->text().trimmed().isEmpty() &&
           !ui_->tradeDateEdit->text().trimmed().isEmpty() &&
           !ui_->effectiveDateEdit->text().trimmed().isEmpty() &&
           !ui_->terminationDateEdit->text().trimmed().isEmpty();
}

// ---------------------------------------------------------------------------
// Save
// ---------------------------------------------------------------------------

void TradeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save while disconnected from server.");
        return;
    }
    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTradeFromUi();
    if (activeForm_ && (createMode_ || instrumentLoaded_))
        activeForm_->writeUiToInstrument();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType,
        hasChanges_ || instrumentHasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;

    trade_.change_reason_code = crSel->reason_code;
    trade_.change_commentary  = crSel->commentary;

    if (activeForm_)
        activeForm_->setChangeReason(crSel->reason_code, crSel->commentary);

    // Save the instrument first when it is new (create mode) or dirty.
    const bool needsInstrumentSave = activeForm_ &&
        (createMode_ || (instrumentHasChanges_ && instrumentLoaded_));

    if (needsInstrumentSave) {
        BOOST_LOG_SEV(lg(), info)
            << "Saving instrument then trade: " << trade_.external_id;
        const auto saved_trade = trade_;
        activeForm_->saveInstrument(
            [this, saved_trade](const std::string& id) {
                instrumentHasChanges_ = false;
                auto t = saved_trade;
                if (!id.empty())
                    t.instrument_id = boost::uuids::string_generator()(id);
                saveTrade(t);
            },
            [this](const QString& err) {
                BOOST_LOG_SEV(lg(), error)
                    << "Instrument save failed: " << err.toStdString();
                emit errorMessage(err);
                MessageBoxHelper::critical(this, "Save Failed",
                    tr("Failed to save instrument:\n%1").arg(err));
            });
    } else {
        BOOST_LOG_SEV(lg(), info) << "Saving trade only: " << trade_.external_id;
        saveTrade(trade_);
    }
}

void TradeDetailDialog::saveTrade(const trading::domain::trade& trade) {
    struct TradeResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<TradeResult>(self);
    connect(watcher, &QFutureWatcher<TradeResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Trade saved successfully";
            QString code = QString::fromStdString(self->trade_.external_id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->tradeSaved(code);
            self->notifySaveSuccess(tr("Trade '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Trade save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run([self, trade]() -> TradeResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};
        trading::messaging::save_trade_request req;
        req.trades.push_back(trade);
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Delete
// ---------------------------------------------------------------------------

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
    if (reply != QMessageBox::Yes) return;

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting trade: " << trade_.external_id;

    struct DeleteResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            emit self->statusMessage(tr("Trade '%1' deleted").arg(code));
            emit self->tradeDeleted(code);
            self->requestClose();
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run([self, id = trade_.id]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};
        trading::messaging::delete_trade_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
