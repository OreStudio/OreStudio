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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#pragma once
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <new>

namespace bench {
inline std::atomic<unsigned long long> g_allocs{0};
inline std::atomic<unsigned long long> g_bytes{0};
inline thread_local bool g_on{false};

struct scope {
    scope() {
        g_allocs = 0;
        g_bytes = 0;
        g_on = true;
    }
    ~scope() { g_on = false; }
    auto allocs() const -> unsigned long long { return g_allocs; }
    auto bytes() const -> unsigned long long { return g_bytes; }
};
}

namespace bench_detail {
inline void* take(std::size_t n, std::size_t align) {
    if (bench::g_on) {
        ++bench::g_allocs;
        bench::g_bytes += n;
    }
    if (align <= __STDCPP_DEFAULT_NEW_ALIGNMENT__) {
        void* p = std::malloc(n ? n : 1);
        if (!p)
            throw std::bad_alloc();
        return p;
    }
    void* p = nullptr;
    if (posix_memalign(&p, align, n ? n : 1) != 0)
        throw std::bad_alloc();
    return p;
}

inline void give(void* p) noexcept {
    std::free(p);
}

inline bool aligned_allocation(const void* p) noexcept {
    return reinterpret_cast<std::uintptr_t>(p) % __STDCPP_DEFAULT_NEW_ALIGNMENT__ == 0;
}
}

void* operator new(std::size_t n) {
    return bench_detail::take(n, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}
void* operator new[](std::size_t n) {
    return bench_detail::take(n, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}
void* operator new(std::size_t n, std::align_val_t a) {
    return bench_detail::take(n, static_cast<std::size_t>(a));
}
void* operator new[](std::size_t n, std::align_val_t a) {
    return bench_detail::take(n, static_cast<std::size_t>(a));
}
void operator delete(void* p) noexcept {
    bench_detail::give(p);
}
void operator delete[](void* p) noexcept {
    bench_detail::give(p);
}
void operator delete(void* p, std::size_t) noexcept {
    bench_detail::give(p);
}
void operator delete[](void* p, std::size_t) noexcept {
    bench_detail::give(p);
}
void operator delete(void* p, std::align_val_t) noexcept {
    bench_detail::give(p);
}
void operator delete[](void* p, std::align_val_t) noexcept {
    bench_detail::give(p);
}
void operator delete(void* p, std::size_t, std::align_val_t) noexcept {
    bench_detail::give(p);
}
void operator delete[](void* p, std::size_t, std::align_val_t) noexcept {
    bench_detail::give(p);
}
