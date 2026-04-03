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
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ---------------------------------------------------------------------------
// Trade type detection helpers
// ---------------------------------------------------------------------------

static bool isFxOptionType(const QString& tradeTypeCode) {
    return tradeTypeCode.contains("Option", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Collar", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Barrier", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Digital", Qt::CaseInsensitive);
}

static bool isSwapRatesExtensionType(const QString& tradeTypeCode) {
    return tradeTypeCode == "ForwardRateAgreement" ||
           tradeTypeCode == "BalanceGuaranteedSwap" ||
           tradeTypeCode == "KnockOutSwap" ||
           tradeTypeCode == "CallableSwap" ||
           tradeTypeCode == "RiskParticipationAgreement" ||
           tradeTypeCode == "InflationSwap";
}

static bool isBondExtensionType(const QString& tradeTypeCode) {
    return tradeTypeCode == "BondFuture" ||
           tradeTypeCode == "BondOption" ||
           tradeTypeCode == "BondTRS" ||
           tradeTypeCode == "Ascot";
}

static bool isCreditExtensionType(const QString& tradeTypeCode) {
    return tradeTypeCode == "CreditDefaultSwapOption" ||
           tradeTypeCode == "IndexCreditDefaultSwap";
}

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
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapCoreTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapRatesTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondOptionalTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondExtensionsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditExtensionsTab), false);
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

    // Swap instrument fields
    connect(ui_->swapTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onSwapTradeTypeChanged);
    connect(ui_->swapCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapStartDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapMaturityDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapFraFixingDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapFraSettlementDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapRpaCounterpartyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapInflationIndexCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapCallableDatesJsonEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapNotionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapLockoutDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->swapBaseCpiSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);

    // Bond instrument fields
    connect(ui_->bondTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onBondTradeTypeChanged);
    connect(ui_->bondIssuerEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondIssueDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondMaturityDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondCallDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondCouponFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondDayCountCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondFutureExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondOptionExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondTrsReturnTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondTrsFundingLegCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondAscotOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondFaceValueSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondCouponRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondSettlementDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondConversionRatioSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->bondOptionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);

    // Credit instrument fields
    connect(ui_->creditTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCreditTradeTypeChanged);
    connect(ui_->creditReferenceEntityEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditTenorEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditStartDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditMaturityDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditDayCountCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditPaymentFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditIndexNameEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditSeniorityEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditRestructuringEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditOptionExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditLinkedAssetCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditNotionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditSpreadSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditRecoveryRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditIndexSeriesSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditOptionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditTrancheAttachmentSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->creditTrancheDetachmentSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);

    // FX instrument fields
    connect(ui_->fxTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFxTradeTypeChanged);
    connect(ui_->fxBoughtCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSoldCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxValueDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSettlementEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxBoughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSoldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxStrikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
}

// ---------------------------------------------------------------------------
// Client manager and reference data loading
// ---------------------------------------------------------------------------

void TradeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadBooks();
    loadCounterparties();
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

// ---------------------------------------------------------------------------
// Public setters
// ---------------------------------------------------------------------------

void TradeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TradeDetailDialog::setTrade(const trading::domain::trade& trade) {
    trade_ = trade;
    updateUiFromTrade();
    selectCurrentBook();
    selectCurrentCounterparty();

    if (trade_.product_type == "fx" && trade_.instrument_id.has_value())
        loadFxInstrument();
    else if (trade_.product_type == "swap" && trade_.instrument_id.has_value())
        loadSwapInstrument();
    else if (trade_.product_type == "bond" && trade_.instrument_id.has_value())
        loadBondInstrument();
    else if (trade_.product_type == "credit" && trade_.instrument_id.has_value())
        loadCreditInstrument();
}

void TradeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->bookCombo->setEnabled(createMode);
    ui_->counterpartyCombo->setEnabled(createMode);
    ui_->externalIdEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode)
        trade_.id = boost::uuids::random_generator()();

    // Instrument tabs never shown in create mode
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapCoreTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapRatesTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondOptionalTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondExtensionsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditExtensionsTab), false);
    ui_->instrumentProvenanceGroup->setVisible(false);

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
    setFxReadOnly(readOnly);
    setSwapReadOnly(readOnly);
    setBondReadOnly(readOnly);
    setCreditReadOnly(readOnly);
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
// FX instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadFxInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const std::string family = trade_.product_type;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct FxResult {
        bool success;
        std::string message;
        trading::domain::fx_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<FxResult>(self);
    connect(watcher, &QFutureWatcher<FxResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load FX instrument: " << result.message;
            return;
        }

        self->fxInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateFxInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> FxResult {
        if (!cm)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* fx =
            std::get_if<trading::domain::fx_instrument>(&r->instrument);
        if (!fx)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *fx};
    }));
}

void TradeDetailDialog::populateFxInstrument() {
    // Block signals while setting values so instrumentHasChanges_ stays false.
    const auto block = [this](bool b) {
        ui_->fxTradeTypeCodeEdit->blockSignals(b);
        ui_->fxBoughtCurrencyEdit->blockSignals(b);
        ui_->fxBoughtAmountSpinBox->blockSignals(b);
        ui_->fxSoldCurrencyEdit->blockSignals(b);
        ui_->fxSoldAmountSpinBox->blockSignals(b);
        ui_->fxValueDateEdit->blockSignals(b);
        ui_->fxSettlementEdit->blockSignals(b);
        ui_->fxOptionTypeEdit->blockSignals(b);
        ui_->fxStrikePriceSpinBox->blockSignals(b);
        ui_->fxExpiryDateEdit->blockSignals(b);
        ui_->fxDescriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->fxTradeTypeCodeEdit->setText(
        QString::fromStdString(fxInstrument_.trade_type_code));
    ui_->fxBoughtCurrencyEdit->setText(
        QString::fromStdString(fxInstrument_.bought_currency));
    ui_->fxBoughtAmountSpinBox->setValue(fxInstrument_.bought_amount);
    ui_->fxSoldCurrencyEdit->setText(
        QString::fromStdString(fxInstrument_.sold_currency));
    ui_->fxSoldAmountSpinBox->setValue(fxInstrument_.sold_amount);
    ui_->fxValueDateEdit->setText(
        QString::fromStdString(fxInstrument_.value_date.value_or("")));
    ui_->fxSettlementEdit->setText(
        QString::fromStdString(fxInstrument_.settlement));
    ui_->fxOptionTypeEdit->setText(
        QString::fromStdString(fxInstrument_.option_type));
    ui_->fxStrikePriceSpinBox->setValue(fxInstrument_.strike_price);
    ui_->fxExpiryDateEdit->setText(
        QString::fromStdString(fxInstrument_.expiry_date));
    ui_->fxDescriptionEdit->setPlainText(
        QString::fromStdString(fxInstrument_.description));
    block(false);

    // Populate instrument provenance section
    ui_->instrumentProvenanceWidget->populate(
        fxInstrument_.version,
        fxInstrument_.modified_by,
        fxInstrument_.performed_by,
        fxInstrument_.recorded_at,
        fxInstrument_.change_reason_code,
        fxInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateFxTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateFxInstrumentFromUi() {
    fxInstrument_.trade_type_code =
        ui_->fxTradeTypeCodeEdit->text().trimmed().toStdString();
    fxInstrument_.bought_currency =
        ui_->fxBoughtCurrencyEdit->text().trimmed().toStdString();
    fxInstrument_.bought_amount = ui_->fxBoughtAmountSpinBox->value();
    fxInstrument_.sold_currency =
        ui_->fxSoldCurrencyEdit->text().trimmed().toStdString();
    fxInstrument_.sold_amount = ui_->fxSoldAmountSpinBox->value();
    {
        const auto vd = ui_->fxValueDateEdit->text().trimmed().toStdString();
        fxInstrument_.value_date = vd.empty() ? std::nullopt : std::optional(vd);
    }
    fxInstrument_.settlement =
        ui_->fxSettlementEdit->text().trimmed().toStdString();
    fxInstrument_.option_type =
        ui_->fxOptionTypeEdit->text().trimmed().toStdString();
    fxInstrument_.strike_price = ui_->fxStrikePriceSpinBox->value();
    fxInstrument_.expiry_date =
        ui_->fxExpiryDateEdit->text().trimmed().toStdString();
    fxInstrument_.description =
        ui_->fxDescriptionEdit->toPlainText().trimmed().toStdString();
    fxInstrument_.modified_by = username_;
    fxInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateFxTabVisibility() {
    const QString tradeType = ui_->fxTradeTypeCodeEdit->text().trimmed();
    const bool showEconomics =
        instrumentLoaded_ && !tradeType.isEmpty();
    const bool showOptions =
        instrumentLoaded_ && isFxOptionType(tradeType);

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), showEconomics);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), showOptions);
}

void TradeDetailDialog::setFxReadOnly(bool readOnly) {
    ui_->fxTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->fxBoughtCurrencyEdit->setReadOnly(readOnly);
    ui_->fxBoughtAmountSpinBox->setReadOnly(readOnly);
    ui_->fxSoldCurrencyEdit->setReadOnly(readOnly);
    ui_->fxSoldAmountSpinBox->setReadOnly(readOnly);
    ui_->fxValueDateEdit->setReadOnly(readOnly);
    ui_->fxSettlementEdit->setReadOnly(readOnly);
    ui_->fxOptionTypeEdit->setReadOnly(readOnly);
    ui_->fxStrikePriceSpinBox->setReadOnly(readOnly);
    ui_->fxExpiryDateEdit->setReadOnly(readOnly);
    ui_->fxDescriptionEdit->setReadOnly(readOnly);
}

// ---------------------------------------------------------------------------
// Swap / Rates instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadSwapInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const std::string family = trade_.product_type;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct SwapResult {
        bool success;
        std::string message;
        trading::domain::instrument instrument;
        std::vector<trading::domain::swap_leg> legs;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<SwapResult>(self);
    connect(watcher, &QFutureWatcher<SwapResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load swap instrument: " << result.message;
            return;
        }

        self->swapInstrument_ = std::move(result.instrument);
        self->swapLegs_ = std::move(result.legs);
        self->instrumentLoaded_ = true;
        self->populateSwapInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> SwapResult {
        if (!cm)
            return {false, "Dialog closed", {}, {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}, {}};
        if (!r->success)
            return {false, r->message, {}, {}};

        const auto* sw =
            std::get_if<trading::messaging::swap_export_result>(&r->instrument);
        if (!sw)
            return {false, "Unexpected instrument type in response", {}, {}};

        return {true, {}, sw->instrument, sw->legs};
    }));
}

void TradeDetailDialog::populateSwapInstrument() {
    const auto block = [this](bool b) {
        ui_->swapTradeTypeCodeEdit->blockSignals(b);
        ui_->swapNotionalSpinBox->blockSignals(b);
        ui_->swapCurrencyEdit->blockSignals(b);
        ui_->swapStartDateEdit->blockSignals(b);
        ui_->swapMaturityDateEdit->blockSignals(b);
        ui_->swapDescriptionEdit->blockSignals(b);
        ui_->swapFraFixingDateEdit->blockSignals(b);
        ui_->swapFraSettlementDateEdit->blockSignals(b);
        ui_->swapLockoutDaysSpinBox->blockSignals(b);
        ui_->swapCallableDatesJsonEdit->blockSignals(b);
        ui_->swapRpaCounterpartyEdit->blockSignals(b);
        ui_->swapInflationIndexCodeEdit->blockSignals(b);
        ui_->swapBaseCpiSpinBox->blockSignals(b);
    };

    block(true);
    ui_->swapTradeTypeCodeEdit->setText(
        QString::fromStdString(swapInstrument_.trade_type_code));
    ui_->swapNotionalSpinBox->setValue(swapInstrument_.notional);
    ui_->swapCurrencyEdit->setText(
        QString::fromStdString(swapInstrument_.currency));
    ui_->swapStartDateEdit->setText(
        QString::fromStdString(swapInstrument_.start_date));
    ui_->swapMaturityDateEdit->setText(
        QString::fromStdString(swapInstrument_.maturity_date));
    ui_->swapDescriptionEdit->setPlainText(
        QString::fromStdString(swapInstrument_.description));
    ui_->swapFraFixingDateEdit->setText(
        QString::fromStdString(swapInstrument_.fra_fixing_date));
    ui_->swapFraSettlementDateEdit->setText(
        QString::fromStdString(swapInstrument_.fra_settlement_date));
    ui_->swapLockoutDaysSpinBox->setValue(
        swapInstrument_.lockout_days.value_or(0));
    ui_->swapCallableDatesJsonEdit->setPlainText(
        QString::fromStdString(swapInstrument_.callable_dates_json));
    ui_->swapRpaCounterpartyEdit->setText(
        QString::fromStdString(swapInstrument_.rpa_counterparty));
    ui_->swapInflationIndexCodeEdit->setText(
        QString::fromStdString(swapInstrument_.inflation_index_code));
    ui_->swapBaseCpiSpinBox->setValue(
        swapInstrument_.base_cpi.value_or(0.0));
    block(false);

    ui_->instrumentProvenanceWidget->populate(
        swapInstrument_.version,
        swapInstrument_.modified_by,
        swapInstrument_.performed_by,
        swapInstrument_.recorded_at,
        swapInstrument_.change_reason_code,
        swapInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateSwapTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateSwapInstrumentFromUi() {
    swapInstrument_.trade_type_code =
        ui_->swapTradeTypeCodeEdit->text().trimmed().toStdString();
    swapInstrument_.notional = ui_->swapNotionalSpinBox->value();
    swapInstrument_.currency =
        ui_->swapCurrencyEdit->text().trimmed().toStdString();
    swapInstrument_.start_date =
        ui_->swapStartDateEdit->text().trimmed().toStdString();
    swapInstrument_.maturity_date =
        ui_->swapMaturityDateEdit->text().trimmed().toStdString();
    swapInstrument_.description =
        ui_->swapDescriptionEdit->toPlainText().trimmed().toStdString();
    swapInstrument_.fra_fixing_date =
        ui_->swapFraFixingDateEdit->text().trimmed().toStdString();
    swapInstrument_.fra_settlement_date =
        ui_->swapFraSettlementDateEdit->text().trimmed().toStdString();
    {
        const int ld = ui_->swapLockoutDaysSpinBox->value();
        swapInstrument_.lockout_days = (ld > 0)
            ? std::optional<int>(ld) : std::nullopt;
    }
    swapInstrument_.callable_dates_json =
        ui_->swapCallableDatesJsonEdit->toPlainText().trimmed().toStdString();
    swapInstrument_.rpa_counterparty =
        ui_->swapRpaCounterpartyEdit->text().trimmed().toStdString();
    swapInstrument_.inflation_index_code =
        ui_->swapInflationIndexCodeEdit->text().trimmed().toStdString();
    {
        const double cpi = ui_->swapBaseCpiSpinBox->value();
        swapInstrument_.base_cpi = (cpi > 0.0)
            ? std::optional<double>(cpi) : std::nullopt;
    }
    swapInstrument_.modified_by = username_;
    swapInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateSwapTabVisibility() {
    const QString tradeType = ui_->swapTradeTypeCodeEdit->text().trimmed();
    const bool showCore = instrumentLoaded_ && !tradeType.isEmpty();
    const bool showRates = instrumentLoaded_ &&
        (isSwapRatesExtensionType(tradeType) ||
         !swapInstrument_.fra_fixing_date.empty() ||
         !swapInstrument_.fra_settlement_date.empty() ||
         swapInstrument_.lockout_days.has_value() ||
         !swapInstrument_.callable_dates_json.empty() ||
         !swapInstrument_.rpa_counterparty.empty() ||
         !swapInstrument_.inflation_index_code.empty() ||
         swapInstrument_.base_cpi.has_value());

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapCoreTab), showCore);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->swapRatesTab), showRates);
}

void TradeDetailDialog::setSwapReadOnly(bool readOnly) {
    ui_->swapTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->swapNotionalSpinBox->setReadOnly(readOnly);
    ui_->swapCurrencyEdit->setReadOnly(readOnly);
    ui_->swapStartDateEdit->setReadOnly(readOnly);
    ui_->swapMaturityDateEdit->setReadOnly(readOnly);
    ui_->swapDescriptionEdit->setReadOnly(readOnly);
    ui_->swapFraFixingDateEdit->setReadOnly(readOnly);
    ui_->swapFraSettlementDateEdit->setReadOnly(readOnly);
    ui_->swapLockoutDaysSpinBox->setReadOnly(readOnly);
    ui_->swapCallableDatesJsonEdit->setReadOnly(readOnly);
    ui_->swapRpaCounterpartyEdit->setReadOnly(readOnly);
    ui_->swapInflationIndexCodeEdit->setReadOnly(readOnly);
    ui_->swapBaseCpiSpinBox->setReadOnly(readOnly);
}

void TradeDetailDialog::saveSwapThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::instrument& instrument,
    const std::vector<trading::domain::swap_leg>& legs) {

    struct SwapSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<SwapSaveResult>(self);
    connect(watcher, &QFutureWatcher<SwapSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Swap instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save swap instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Swap instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument, legs]() -> SwapSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_instrument_request req;
        req.data = instrument;
        req.legs = legs;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Bond instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadBondInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const std::string family = trade_.product_type;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct BondResult {
        bool success;
        std::string message;
        trading::domain::bond_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<BondResult>(self);
    connect(watcher, &QFutureWatcher<BondResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load bond instrument: " << result.message;
            return;
        }

        self->bondInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateBondInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> BondResult {
        if (!cm)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* bond =
            std::get_if<trading::domain::bond_instrument>(&r->instrument);
        if (!bond)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *bond};
    }));
}

void TradeDetailDialog::populateBondInstrument() {
    const auto block = [this](bool b) {
        ui_->bondTradeTypeCodeEdit->blockSignals(b);
        ui_->bondIssuerEdit->blockSignals(b);
        ui_->bondCurrencyEdit->blockSignals(b);
        ui_->bondFaceValueSpinBox->blockSignals(b);
        ui_->bondCouponRateSpinBox->blockSignals(b);
        ui_->bondCouponFrequencyCodeEdit->blockSignals(b);
        ui_->bondDayCountCodeEdit->blockSignals(b);
        ui_->bondIssueDateEdit->blockSignals(b);
        ui_->bondMaturityDateEdit->blockSignals(b);
        ui_->bondSettlementDaysSpinBox->blockSignals(b);
        ui_->bondCallDateEdit->blockSignals(b);
        ui_->bondConversionRatioSpinBox->blockSignals(b);
        ui_->bondDescriptionEdit->blockSignals(b);
        ui_->bondFutureExpiryDateEdit->blockSignals(b);
        ui_->bondOptionTypeEdit->blockSignals(b);
        ui_->bondOptionExpiryDateEdit->blockSignals(b);
        ui_->bondOptionStrikeSpinBox->blockSignals(b);
        ui_->bondTrsReturnTypeEdit->blockSignals(b);
        ui_->bondTrsFundingLegCodeEdit->blockSignals(b);
        ui_->bondAscotOptionTypeEdit->blockSignals(b);
    };

    block(true);
    ui_->bondTradeTypeCodeEdit->setText(
        QString::fromStdString(bondInstrument_.trade_type_code));
    ui_->bondIssuerEdit->setText(
        QString::fromStdString(bondInstrument_.issuer));
    ui_->bondCurrencyEdit->setText(
        QString::fromStdString(bondInstrument_.currency));
    ui_->bondFaceValueSpinBox->setValue(bondInstrument_.face_value);
    ui_->bondCouponRateSpinBox->setValue(bondInstrument_.coupon_rate);
    ui_->bondCouponFrequencyCodeEdit->setText(
        QString::fromStdString(bondInstrument_.coupon_frequency_code));
    ui_->bondDayCountCodeEdit->setText(
        QString::fromStdString(bondInstrument_.day_count_code));
    ui_->bondIssueDateEdit->setText(
        QString::fromStdString(bondInstrument_.issue_date));
    ui_->bondMaturityDateEdit->setText(
        QString::fromStdString(bondInstrument_.maturity_date));
    ui_->bondSettlementDaysSpinBox->setValue(bondInstrument_.settlement_days);
    ui_->bondCallDateEdit->setText(
        QString::fromStdString(bondInstrument_.call_date));
    ui_->bondConversionRatioSpinBox->setValue(bondInstrument_.conversion_ratio);
    ui_->bondDescriptionEdit->setPlainText(
        QString::fromStdString(bondInstrument_.description));
    ui_->bondFutureExpiryDateEdit->setText(
        QString::fromStdString(bondInstrument_.future_expiry_date));
    ui_->bondOptionTypeEdit->setText(
        QString::fromStdString(bondInstrument_.option_type));
    ui_->bondOptionExpiryDateEdit->setText(
        QString::fromStdString(bondInstrument_.option_expiry_date));
    ui_->bondOptionStrikeSpinBox->setValue(
        bondInstrument_.option_strike.value_or(0.0));
    ui_->bondTrsReturnTypeEdit->setText(
        QString::fromStdString(bondInstrument_.trs_return_type));
    ui_->bondTrsFundingLegCodeEdit->setText(
        QString::fromStdString(bondInstrument_.trs_funding_leg_code));
    ui_->bondAscotOptionTypeEdit->setText(
        QString::fromStdString(bondInstrument_.ascot_option_type));
    block(false);

    ui_->instrumentProvenanceWidget->populate(
        bondInstrument_.version,
        bondInstrument_.modified_by,
        bondInstrument_.performed_by,
        bondInstrument_.recorded_at,
        bondInstrument_.change_reason_code,
        bondInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateBondTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateBondInstrumentFromUi() {
    bondInstrument_.trade_type_code =
        ui_->bondTradeTypeCodeEdit->text().trimmed().toStdString();
    bondInstrument_.issuer =
        ui_->bondIssuerEdit->text().trimmed().toStdString();
    bondInstrument_.currency =
        ui_->bondCurrencyEdit->text().trimmed().toStdString();
    bondInstrument_.face_value = ui_->bondFaceValueSpinBox->value();
    bondInstrument_.coupon_rate = ui_->bondCouponRateSpinBox->value();
    bondInstrument_.coupon_frequency_code =
        ui_->bondCouponFrequencyCodeEdit->text().trimmed().toStdString();
    bondInstrument_.day_count_code =
        ui_->bondDayCountCodeEdit->text().trimmed().toStdString();
    bondInstrument_.issue_date =
        ui_->bondIssueDateEdit->text().trimmed().toStdString();
    bondInstrument_.maturity_date =
        ui_->bondMaturityDateEdit->text().trimmed().toStdString();
    bondInstrument_.settlement_days = ui_->bondSettlementDaysSpinBox->value();
    bondInstrument_.call_date =
        ui_->bondCallDateEdit->text().trimmed().toStdString();
    bondInstrument_.conversion_ratio = ui_->bondConversionRatioSpinBox->value();
    bondInstrument_.description =
        ui_->bondDescriptionEdit->toPlainText().trimmed().toStdString();
    bondInstrument_.future_expiry_date =
        ui_->bondFutureExpiryDateEdit->text().trimmed().toStdString();
    bondInstrument_.option_type =
        ui_->bondOptionTypeEdit->text().trimmed().toStdString();
    bondInstrument_.option_expiry_date =
        ui_->bondOptionExpiryDateEdit->text().trimmed().toStdString();
    {
        const double s = ui_->bondOptionStrikeSpinBox->value();
        bondInstrument_.option_strike = (s > 0.0)
            ? std::optional<double>(s) : std::nullopt;
    }
    bondInstrument_.trs_return_type =
        ui_->bondTrsReturnTypeEdit->text().trimmed().toStdString();
    bondInstrument_.trs_funding_leg_code =
        ui_->bondTrsFundingLegCodeEdit->text().trimmed().toStdString();
    bondInstrument_.ascot_option_type =
        ui_->bondAscotOptionTypeEdit->text().trimmed().toStdString();
    bondInstrument_.modified_by = username_;
    bondInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateBondTabVisibility() {
    const QString tradeType = ui_->bondTradeTypeCodeEdit->text().trimmed();
    const bool showCore = instrumentLoaded_ && !tradeType.isEmpty();
    const bool showExtensions = instrumentLoaded_ &&
        (isBondExtensionType(tradeType) ||
         !bondInstrument_.future_expiry_date.empty() ||
         !bondInstrument_.option_type.empty() ||
         !bondInstrument_.trs_return_type.empty() ||
         !bondInstrument_.ascot_option_type.empty());

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondEconomicsTab), showCore);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondOptionalTab), showCore);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->bondExtensionsTab), showExtensions);
}

void TradeDetailDialog::setBondReadOnly(bool readOnly) {
    ui_->bondTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->bondIssuerEdit->setReadOnly(readOnly);
    ui_->bondCurrencyEdit->setReadOnly(readOnly);
    ui_->bondFaceValueSpinBox->setReadOnly(readOnly);
    ui_->bondCouponRateSpinBox->setReadOnly(readOnly);
    ui_->bondCouponFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->bondDayCountCodeEdit->setReadOnly(readOnly);
    ui_->bondIssueDateEdit->setReadOnly(readOnly);
    ui_->bondMaturityDateEdit->setReadOnly(readOnly);
    ui_->bondSettlementDaysSpinBox->setReadOnly(readOnly);
    ui_->bondCallDateEdit->setReadOnly(readOnly);
    ui_->bondConversionRatioSpinBox->setReadOnly(readOnly);
    ui_->bondDescriptionEdit->setReadOnly(readOnly);
    ui_->bondFutureExpiryDateEdit->setReadOnly(readOnly);
    ui_->bondOptionTypeEdit->setReadOnly(readOnly);
    ui_->bondOptionExpiryDateEdit->setReadOnly(readOnly);
    ui_->bondOptionStrikeSpinBox->setReadOnly(readOnly);
    ui_->bondTrsReturnTypeEdit->setReadOnly(readOnly);
    ui_->bondTrsFundingLegCodeEdit->setReadOnly(readOnly);
    ui_->bondAscotOptionTypeEdit->setReadOnly(readOnly);
}

void TradeDetailDialog::saveBondThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::bond_instrument& instrument) {

    struct BondSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<BondSaveResult>(self);
    connect(watcher, &QFutureWatcher<BondSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Bond instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save bond instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Bond instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument]() -> BondSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_bond_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Credit instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadCreditInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const std::string family = trade_.product_type;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct CreditResult {
        bool success;
        std::string message;
        trading::domain::credit_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CreditResult>(self);
    connect(watcher, &QFutureWatcher<CreditResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load credit instrument: " << result.message;
            return;
        }

        self->creditInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateCreditInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> CreditResult {
        if (!cm)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* credit =
            std::get_if<trading::domain::credit_instrument>(&r->instrument);
        if (!credit)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *credit};
    }));
}

void TradeDetailDialog::populateCreditInstrument() {
    const auto block = [this](bool b) {
        ui_->creditTradeTypeCodeEdit->blockSignals(b);
        ui_->creditReferenceEntityEdit->blockSignals(b);
        ui_->creditCurrencyEdit->blockSignals(b);
        ui_->creditNotionalSpinBox->blockSignals(b);
        ui_->creditSpreadSpinBox->blockSignals(b);
        ui_->creditRecoveryRateSpinBox->blockSignals(b);
        ui_->creditTenorEdit->blockSignals(b);
        ui_->creditStartDateEdit->blockSignals(b);
        ui_->creditMaturityDateEdit->blockSignals(b);
        ui_->creditDayCountCodeEdit->blockSignals(b);
        ui_->creditPaymentFrequencyCodeEdit->blockSignals(b);
        ui_->creditIndexNameEdit->blockSignals(b);
        ui_->creditIndexSeriesSpinBox->blockSignals(b);
        ui_->creditSeniorityEdit->blockSignals(b);
        ui_->creditRestructuringEdit->blockSignals(b);
        ui_->creditDescriptionEdit->blockSignals(b);
        ui_->creditOptionTypeEdit->blockSignals(b);
        ui_->creditOptionExpiryDateEdit->blockSignals(b);
        ui_->creditOptionStrikeSpinBox->blockSignals(b);
        ui_->creditLinkedAssetCodeEdit->blockSignals(b);
        ui_->creditTrancheAttachmentSpinBox->blockSignals(b);
        ui_->creditTrancheDetachmentSpinBox->blockSignals(b);
    };

    block(true);
    ui_->creditTradeTypeCodeEdit->setText(
        QString::fromStdString(creditInstrument_.trade_type_code));
    ui_->creditReferenceEntityEdit->setText(
        QString::fromStdString(creditInstrument_.reference_entity));
    ui_->creditCurrencyEdit->setText(
        QString::fromStdString(creditInstrument_.currency));
    ui_->creditNotionalSpinBox->setValue(creditInstrument_.notional);
    ui_->creditSpreadSpinBox->setValue(creditInstrument_.spread);
    ui_->creditRecoveryRateSpinBox->setValue(creditInstrument_.recovery_rate);
    ui_->creditTenorEdit->setText(
        QString::fromStdString(creditInstrument_.tenor));
    ui_->creditStartDateEdit->setText(
        QString::fromStdString(creditInstrument_.start_date));
    ui_->creditMaturityDateEdit->setText(
        QString::fromStdString(creditInstrument_.maturity_date));
    ui_->creditDayCountCodeEdit->setText(
        QString::fromStdString(creditInstrument_.day_count_code));
    ui_->creditPaymentFrequencyCodeEdit->setText(
        QString::fromStdString(creditInstrument_.payment_frequency_code));
    ui_->creditIndexNameEdit->setText(
        QString::fromStdString(creditInstrument_.index_name));
    ui_->creditIndexSeriesSpinBox->setValue(creditInstrument_.index_series);
    ui_->creditSeniorityEdit->setText(
        QString::fromStdString(creditInstrument_.seniority));
    ui_->creditRestructuringEdit->setText(
        QString::fromStdString(creditInstrument_.restructuring));
    ui_->creditDescriptionEdit->setPlainText(
        QString::fromStdString(creditInstrument_.description));
    ui_->creditOptionTypeEdit->setText(
        QString::fromStdString(creditInstrument_.option_type));
    ui_->creditOptionExpiryDateEdit->setText(
        QString::fromStdString(creditInstrument_.option_expiry_date));
    ui_->creditOptionStrikeSpinBox->setValue(
        creditInstrument_.option_strike.value_or(0.0));
    ui_->creditLinkedAssetCodeEdit->setText(
        QString::fromStdString(creditInstrument_.linked_asset_code));
    ui_->creditTrancheAttachmentSpinBox->setValue(
        creditInstrument_.tranche_attachment.value_or(0.0));
    ui_->creditTrancheDetachmentSpinBox->setValue(
        creditInstrument_.tranche_detachment.value_or(0.0));
    block(false);

    ui_->instrumentProvenanceWidget->populate(
        creditInstrument_.version,
        creditInstrument_.modified_by,
        creditInstrument_.performed_by,
        creditInstrument_.recorded_at,
        creditInstrument_.change_reason_code,
        creditInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateCreditTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateCreditInstrumentFromUi() {
    creditInstrument_.trade_type_code =
        ui_->creditTradeTypeCodeEdit->text().trimmed().toStdString();
    creditInstrument_.reference_entity =
        ui_->creditReferenceEntityEdit->text().trimmed().toStdString();
    creditInstrument_.currency =
        ui_->creditCurrencyEdit->text().trimmed().toStdString();
    creditInstrument_.notional = ui_->creditNotionalSpinBox->value();
    creditInstrument_.spread = ui_->creditSpreadSpinBox->value();
    creditInstrument_.recovery_rate = ui_->creditRecoveryRateSpinBox->value();
    creditInstrument_.tenor =
        ui_->creditTenorEdit->text().trimmed().toStdString();
    creditInstrument_.start_date =
        ui_->creditStartDateEdit->text().trimmed().toStdString();
    creditInstrument_.maturity_date =
        ui_->creditMaturityDateEdit->text().trimmed().toStdString();
    creditInstrument_.day_count_code =
        ui_->creditDayCountCodeEdit->text().trimmed().toStdString();
    creditInstrument_.payment_frequency_code =
        ui_->creditPaymentFrequencyCodeEdit->text().trimmed().toStdString();
    creditInstrument_.index_name =
        ui_->creditIndexNameEdit->text().trimmed().toStdString();
    creditInstrument_.index_series = ui_->creditIndexSeriesSpinBox->value();
    creditInstrument_.seniority =
        ui_->creditSeniorityEdit->text().trimmed().toStdString();
    creditInstrument_.restructuring =
        ui_->creditRestructuringEdit->text().trimmed().toStdString();
    creditInstrument_.description =
        ui_->creditDescriptionEdit->toPlainText().trimmed().toStdString();
    creditInstrument_.option_type =
        ui_->creditOptionTypeEdit->text().trimmed().toStdString();
    creditInstrument_.option_expiry_date =
        ui_->creditOptionExpiryDateEdit->text().trimmed().toStdString();
    {
        const double s = ui_->creditOptionStrikeSpinBox->value();
        creditInstrument_.option_strike = (s > 0.0)
            ? std::optional<double>(s) : std::nullopt;
    }
    creditInstrument_.linked_asset_code =
        ui_->creditLinkedAssetCodeEdit->text().trimmed().toStdString();
    {
        const double a = ui_->creditTrancheAttachmentSpinBox->value();
        creditInstrument_.tranche_attachment = (a > 0.0)
            ? std::optional<double>(a) : std::nullopt;
    }
    {
        const double d = ui_->creditTrancheDetachmentSpinBox->value();
        creditInstrument_.tranche_detachment = (d > 0.0)
            ? std::optional<double>(d) : std::nullopt;
    }
    creditInstrument_.modified_by = username_;
    creditInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateCreditTabVisibility() {
    const QString tradeType = ui_->creditTradeTypeCodeEdit->text().trimmed();
    const bool showCore = instrumentLoaded_ && !tradeType.isEmpty();
    const bool showExtensions = instrumentLoaded_ &&
        (isCreditExtensionType(tradeType) ||
         !creditInstrument_.option_type.empty() ||
         !creditInstrument_.linked_asset_code.empty() ||
         creditInstrument_.tranche_attachment.has_value() ||
         creditInstrument_.tranche_detachment.has_value());

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditEconomicsTab), showCore);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->creditExtensionsTab), showExtensions);
}

void TradeDetailDialog::setCreditReadOnly(bool readOnly) {
    ui_->creditTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->creditReferenceEntityEdit->setReadOnly(readOnly);
    ui_->creditCurrencyEdit->setReadOnly(readOnly);
    ui_->creditNotionalSpinBox->setReadOnly(readOnly);
    ui_->creditSpreadSpinBox->setReadOnly(readOnly);
    ui_->creditRecoveryRateSpinBox->setReadOnly(readOnly);
    ui_->creditTenorEdit->setReadOnly(readOnly);
    ui_->creditStartDateEdit->setReadOnly(readOnly);
    ui_->creditMaturityDateEdit->setReadOnly(readOnly);
    ui_->creditDayCountCodeEdit->setReadOnly(readOnly);
    ui_->creditPaymentFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->creditIndexNameEdit->setReadOnly(readOnly);
    ui_->creditIndexSeriesSpinBox->setReadOnly(readOnly);
    ui_->creditSeniorityEdit->setReadOnly(readOnly);
    ui_->creditRestructuringEdit->setReadOnly(readOnly);
    ui_->creditDescriptionEdit->setReadOnly(readOnly);
    ui_->creditOptionTypeEdit->setReadOnly(readOnly);
    ui_->creditOptionExpiryDateEdit->setReadOnly(readOnly);
    ui_->creditOptionStrikeSpinBox->setReadOnly(readOnly);
    ui_->creditLinkedAssetCodeEdit->setReadOnly(readOnly);
    ui_->creditTrancheAttachmentSpinBox->setReadOnly(readOnly);
    ui_->creditTrancheDetachmentSpinBox->setReadOnly(readOnly);
}

void TradeDetailDialog::saveCreditThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::credit_instrument& instrument) {

    struct CreditSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CreditSaveResult>(self);
    connect(watcher, &QFutureWatcher<CreditSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Credit instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save credit instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Credit instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument]() -> CreditSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_credit_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
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

void TradeDetailDialog::onFxTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateFxTabVisibility();
        updateSaveButtonState();
    }
}

void TradeDetailDialog::onSwapTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateSwapTabVisibility();
        updateSaveButtonState();
    }
}

void TradeDetailDialog::onBondTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateBondTabVisibility();
        updateSaveButtonState();
    }
}

void TradeDetailDialog::onCreditTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateCreditTabVisibility();
        updateSaveButtonState();
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
    if (instrumentLoaded_) {
        if (trade_.product_type == "fx")
            updateFxInstrumentFromUi();
        else if (trade_.product_type == "swap")
            updateSwapInstrumentFromUi();
        else if (trade_.product_type == "bond")
            updateBondInstrumentFromUi();
        else if (trade_.product_type == "credit")
            updateCreditInstrumentFromUi();
    }

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType,
        hasChanges_ || instrumentHasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;

    trade_.change_reason_code = crSel->reason_code;
    trade_.change_commentary  = crSel->commentary;

    if (instrumentLoaded_) {
        // Instrument change reason matches the trade change reason.
        if (trade_.product_type == "fx") {
            fxInstrument_.change_reason_code = crSel->reason_code;
            fxInstrument_.change_commentary  = crSel->commentary;
        } else if (trade_.product_type == "swap") {
            swapInstrument_.change_reason_code = crSel->reason_code;
            swapInstrument_.change_commentary  = crSel->commentary;
        } else if (trade_.product_type == "bond") {
            bondInstrument_.change_reason_code = crSel->reason_code;
            bondInstrument_.change_commentary  = crSel->commentary;
        } else if (trade_.product_type == "credit") {
            creditInstrument_.change_reason_code = crSel->reason_code;
            creditInstrument_.change_commentary  = crSel->commentary;
        }
    }

    if (instrumentHasChanges_ && instrumentLoaded_) {
        if (trade_.product_type == "fx") {
            BOOST_LOG_SEV(lg(), info) << "Saving FX instrument then trade: "
                                      << trade_.external_id;
            saveFxThenTrade(trade_, fxInstrument_);
        } else if (trade_.product_type == "swap") {
            BOOST_LOG_SEV(lg(), info) << "Saving swap instrument then trade: "
                                      << trade_.external_id;
            saveSwapThenTrade(trade_, swapInstrument_, swapLegs_);
        } else if (trade_.product_type == "bond") {
            BOOST_LOG_SEV(lg(), info) << "Saving bond instrument then trade: "
                                      << trade_.external_id;
            saveBondThenTrade(trade_, bondInstrument_);
        } else if (trade_.product_type == "credit") {
            BOOST_LOG_SEV(lg(), info) << "Saving credit instrument then trade: "
                                      << trade_.external_id;
            saveCreditThenTrade(trade_, creditInstrument_);
        }
    } else {
        BOOST_LOG_SEV(lg(), info) << "Saving trade only: " << trade_.external_id;
        saveTrade(trade_);
    }
}

void TradeDetailDialog::saveFxThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::fx_instrument& instrument) {

    struct FxSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<FxSaveResult>(self);
    connect(watcher, &QFutureWatcher<FxSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "FX instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save FX instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument]() -> FxSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_fx_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
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
