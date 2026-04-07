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
#include "ores.qt/CompositeInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CompositeInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/CompositeLegsWidget.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CompositeInstrumentForm::CompositeInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::CompositeInstrumentForm) {
    ui_->setupUi(this);
    setupConnections();
}

CompositeInstrumentForm::~CompositeInstrumentForm() = default;

void CompositeInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCombo, &QComboBox::currentTextChanged,
            this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged,
            this, markChanged);
    connect(ui_->legsWidget, &CompositeLegsWidget::legsChanged,
            this, markChanged);
}

void CompositeInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void CompositeInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void CompositeInstrumentForm::clear() {
    instrument_ = trading::domain::composite_instrument{};
    legs_.clear();
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void CompositeInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    const auto idx = ui_->tradeTypeCombo->findText(code.trimmed());
    if (idx >= 0)
        ui_->tradeTypeCombo->setCurrentIndex(idx);
}

void CompositeInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCombo->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->legsWidget->setReadOnly(readOnly);
}

bool CompositeInstrumentForm::isDirty() const { return dirty_; }
bool CompositeInstrumentForm::isLoaded() const { return loaded_; }

void CompositeInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void CompositeInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCombo->currentText().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    legs_ = ui_->legsWidget->legs();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CompositeInstrumentForm::loadInstrument(const std::string& id) {
    if (!clientManager_) return;

    struct LoadResult {
        bool success;
        std::string message;
        trading::domain::composite_instrument instrument;
        std::vector<trading::domain::composite_leg> legs;
    };

    QPointer<CompositeInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<LoadResult>(self);
    connect(watcher, &QFutureWatcher<LoadResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load composite instrument: " << result.message;
            emit self->loadFailed(QString::fromStdString(result.message));
            return;
        }

        self->instrument_ = std::move(result.instrument);
        self->legs_ = std::move(result.legs);
        self->loaded_ = true;
        self->dirty_ = false;
        self->populateFromInstrument();
        self->emitProvenance();
        emit self->instrumentLoaded();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, id]() -> LoadResult {
        if (!cm)
            return {false, "Dialog closed", {}, {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = trading::domain::product_type::composite;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}, {}};
        if (!r->success)
            return {false, r->message, {}, {}};

        const auto* comp =
            std::get_if<trading::messaging::composite_export_result>(&r->instrument);
        if (!comp)
            return {false, "Unexpected instrument type in response", {}, {}};

        return {true, {}, comp->instrument, comp->legs};
    }));
}

void CompositeInstrumentForm::populateFromInstrument() {
    ui_->tradeTypeCombo->blockSignals(true);
    ui_->descriptionEdit->blockSignals(true);
    ui_->legsWidget->blockSignals(true);

    const auto idx = ui_->tradeTypeCombo->findText(
        QString::fromStdString(instrument_.trade_type_code));
    if (idx >= 0)
        ui_->tradeTypeCombo->setCurrentIndex(idx);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->legsWidget->setLegs(legs_);

    ui_->tradeTypeCombo->blockSignals(false);
    ui_->descriptionEdit->blockSignals(false);
    ui_->legsWidget->blockSignals(false);
}

void CompositeInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void CompositeInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void CompositeInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<CompositeInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher,
         on_success = std::move(on_success),
         on_failure = std::move(on_failure)]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Composite instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Composite instrument saved";
        self->dirty_ = false;
        self->emitProvenance();
        on_success(boost::uuids::to_string(self->instrument_.id));
    });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    auto legs = legs_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument = std::move(instrument),
         legs = std::move(legs)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_composite_instrument_request req;
        req.data = instrument;
        req.legs = legs;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
