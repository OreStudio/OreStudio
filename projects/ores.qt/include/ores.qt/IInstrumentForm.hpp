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
#ifndef ORES_QT_I_INSTRUMENT_FORM_HPP
#define ORES_QT_I_INSTRUMENT_FORM_HPP

#include <chrono>
#include <functional>
#include <string>
#include <QString>
#include <QWidget>

namespace ores::qt {

class ClientManager;

/**
 * @brief Audit metadata snapshot reported by an @c IInstrumentForm so the
 *        outer dialog can refresh its (shared) instrument provenance widget.
 */
struct InstrumentProvenance {
    int version = 0;
    std::string modified_by;
    std::string performed_by;
    std::chrono::system_clock::time_point recorded_at;
    std::string change_reason_code;
    std::string change_commentary;
};

/**
 * @brief Pure-virtual interface implemented by every per-family instrument
 *        form widget hosted in @c TradeDetailDialog.
 *
 * Each subclass owns the UI, the in-flight domain object, and the NATS
 * load/save plumbing for exactly one product family. The outer dialog only
 * sees this interface and never references family-specific types.
 *
 * Lifecycle:
 *
 *  1. Dialog construction: instantiate the form via the registry, call
 *     @ref setClientManager and @ref setUsername.
 *  2. Edit mode: @ref loadInstrument is called with the linked instrument
 *     id; the form fetches and populates fields asynchronously.
 *  3. Create mode: @ref clear is called and the form starts blank.
 *  4. The user picks a trade type → dialog calls @ref setTradeType so the
 *     form can show or hide its options/extension sub-sections (driven by
 *     the @c has_options / @c has_extension flags on the @c trade_type
 *     reference data).
 *  5. On save: dialog calls @ref writeUiToInstrument, then
 *     @ref setChangeReason, then @ref saveInstrument with success and
 *     failure callbacks. On success the dialog continues to @c saveTrade;
 *     on failure the trade save is aborted.
 */
class IInstrumentForm : public QWidget {
    Q_OBJECT
public:
    using QWidget::QWidget;
    ~IInstrumentForm() override = default;

    /// Inject the NATS client used for load and save round-trips.
    virtual void setClientManager(ClientManager* cm) = 0;

    /// Inject the username stamped on every saved instrument.
    virtual void setUsername(const std::string& username) = 0;

    /**
     * @brief Asynchronously fetch the instrument identified by @p id.
     *
     * On success, populates the form fields and emits @ref instrumentLoaded
     * and @ref provenanceChanged. On failure, emits @ref loadFailed.
     */
    virtual void loadInstrument(const std::string& id) = 0;

    /// Reset the form to a blank state ready for create mode.
    virtual void clear() = 0;

    /**
     * @brief Inform the form which trade type is currently selected.
     *
     * @param code           The trade type code (e.g. "FxOption").
     * @param has_options    Reveal the family's options sub-section.
     * @param has_extension  Reveal the family's extension sub-section.
     *
     * The form decides how to lay out its sub-sections based on these
     * flags — typically by toggling the visibility of internal tabs.
     */
    virtual void setTradeType(const QString& code,
        bool has_options, bool has_extension) = 0;

    /// Toggle read-only on every editable widget.
    virtual void setReadOnly(bool readOnly) = 0;

    /// Has the user edited any field since the last load or save?
    virtual bool isDirty() const = 0;

    /// True after the asynchronous load has populated the form.
    virtual bool isLoaded() const = 0;

    /**
     * @brief Stamp the change reason on the in-flight instrument before
     *        the dialog calls @ref saveInstrument.
     *
     * The dialog gathers a single change reason once per save and applies
     * it identically to both the trade and the instrument so that the two
     * audit trails stay aligned.
     */
    virtual void setChangeReason(
        const std::string& code, const std::string& commentary) = 0;

    /// Pull the current UI values into the in-flight domain object.
    virtual void writeUiToInstrument() = 0;

    /**
     * @brief Save the in-flight instrument via the family's NATS request.
     *
     * @param on_success  Invoked on the Qt thread with the saved instrument
     *                    id (so the dialog can populate @c trade.instrument_id
     *                    in create mode and continue to the trade save).
     * @param on_failure  Invoked on the Qt thread with a human-readable
     *                    error message; the dialog should abort the trade
     *                    save and surface the error to the user.
     */
    virtual void saveInstrument(
        std::function<void(const std::string& id)> on_success,
        std::function<void(const QString& error)> on_failure) = 0;

signals:
    /// Emitted when any field changes (after the initial load).
    void changed();

    /// Emitted once the asynchronous load has populated the form.
    void instrumentLoaded();

    /// Emitted on a load failure with the server's error message.
    void loadFailed(const QString& error);

    /**
     * @brief Emitted with the latest audit metadata after load and save.
     *
     * The outer dialog uses this to refresh its shared
     * @c instrumentProvenanceWidget — the widget itself stays in the
     * dialog so it remains family-agnostic.
     */
    void provenanceChanged(const InstrumentProvenance& provenance);
};

}

#endif
